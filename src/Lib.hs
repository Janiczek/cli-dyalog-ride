module Lib (run) where

import Control.Concurrent (forkIO, myThreadId)
import qualified Control.Exception as E
import Control.Monad (forever)
import Data.Binary.Get as G (getInt32be, runGet)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack, replace, unpack)
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    listen,
    setSocketOption,
    socket,
    socketToHandle,
    tupleToHostAddress,
  )
import Safe (tailSafe)
import System.Exit (ExitCode (ExitSuccess))
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    IOMode (ReadWriteMode, WriteMode),
    hSetBuffering,
    hWaitForInput,
    stdin,
    stdout,
    withFile,
  )
import System.Posix.Signals
  ( Handler (Catch),
    installHandler,
    sigINT,
    sigTERM,
  )
import System.Process
  ( CreateProcess (env, std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe, UseHandle),
    createProcess_,
    proc,
    terminateProcess,
  )

serveTcpServer :: IO Socket
serveTcpServer = do
  putStrLn "1) Starting up the TCP server"
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 8000 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 1
  return sock

spawnDyalog :: IO ProcessHandle
spawnDyalog = do
  putStrLn "2) Spawning Dyalog"
  withFile "/dev/null" WriteMode $ \devNullHandle -> do
    (_, _, _, dyalogHandle) <-
      createProcess_
        "dyalog process"
        (proc dyalogPath dyalogArgs)
          { --cwd = Just cwd,
            env =
              Just
                [ ("CLASSICMODE", "1"),
                  ("SINGLETRACE", "1"),
                  ("RIDE_INIT", "CONNECT:127.0.0.1:8000"),
                  ("RIDE_SPAWNED", "1"),
                  ("APLK0", "default")
                ],
            std_in = CreatePipe, --Inherit,
            std_out = UseHandle devNullHandle,
            std_err = UseHandle devNullHandle
          }
    return dyalogHandle
  where
    dyalogPath = "/home/martin/.lib/dyalog/opt/mdyalog/18.0/64/unicode/dyalog"
    dyalogArgs = ["+s", "-q"]

sendToDyalog :: Handle -> String -> IO ()
sendToDyalog handle string =
  let message = BB.stringUtf8 string
      messageBS = BB.toLazyByteString message
      messageLength = B.length messageBS
      everythingLength = fromIntegral (8 + messageLength) :: Int32
      everything =
        BB.int32BE everythingLength
          <> BB.stringUtf8 "RIDE"
          <> message
   in do
        putStrLn $ "Sending: " ++ string
        BB.hPutBuilder handle everything

readFromDyalog :: Handle -> IO String
readFromDyalog handle = do
  hWaitForInput handle 4
  lengthBS <- B.hGet handle 4
  let length = fromIntegral $ G.runGet G.getInt32be lengthBS
  hWaitForInput handle length
  B.hGet handle 4 -- "RIDE"
  responseBS <- B.hGet handle (length - 8) -- minus length minus RIDE
  let response = BC.unpack responseBS
  return response

sendUserInput :: Handle -> String -> IO ()
sendUserInput handle string =
  sendToDyalog handle wrappedString
  where
    wrappedString = "[\"Execute\",{\"text\":\"" ++ escapedString ++ "\",\"trace\":0}]"
    escapedString = unpack $ replace (pack "\"") (pack "\\\"") (pack string)

listenToConnection :: Socket -> IO ()
listenToConnection sock = do
  putStrLn "3) Waiting for a TCP connection from Dyalog"
  (socket, addr) <- accept sock
  putStrLn $ "4) Accepted a connection:" ++ show (socket, addr)
  handle <- socketToHandle socket ReadWriteMode
  hSetBuffering handle NoBuffering
  sendToDyalog handle "SupportedProtocols=2"
  sendToDyalog handle "UsingProtocol=2"
  sendToDyalog handle "[\"Identify\",{\"identity\":1}]"
  sendToDyalog handle "[\"Connect\",{\"remoteId\":2}]"
  sendToDyalog handle "[\"GetWindowLayout\",{}]"
  forkIO $ handleUserInput handle
  forever $ do
    putStrLn "Waiting for a message"
    message <- readFromDyalog handle
    putStrLn $ "Received: " ++ message

handleUserInput :: Handle -> IO ()
handleUserInput handle = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  go False ""
  where
    go :: Bool -> String -> IO ()
    go backslashMode inputRev = do
      char <- getChar
      case char of
        '\n' -> do
          sendUserInput handle (reverse inputRev)
          go False ""
        '\b' -> backspace inputRev
        '\DEL' -> backspace inputRev
        '`' -> go True inputRev
        _ ->
          if backslashMode
            then do
              let newChar = fromMaybe ' ' $ Map.lookup char aplBackslashChars
              putStr $ "\b\b" ++ [newChar] ++ " \b"
              go False (newChar : inputRev)
            else do
              go False (char : inputRev)
    backspace inputRev = do
      if null inputRev
        then do
          putStr "\b \b\b \b"
          go False inputRev
        else do
          putStr "\b \b\b \b\b \b"
          go False (tailSafe inputRev)

aplBackslashChars :: Map Char Char
aplBackslashChars =
  Map.fromList $
    zip
      "`1234567890-=qwertyuiop[]asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+ETIOP{}|JKL:Z<>?\\\""
      "⋄¨¯<≤=≥>≠∨∧×÷?⍵∊⍴~↑↓⍳○*←→⍺⌈⌊_∇∆∘'⎕⍎⍕⊂⊃∩∪⊥⊤|⍝⍀⌿⌺⌶⍫⍒⍋⌽⍉⊖⍟⍱⍲!⌹⍷⍨⍸⍥⍣⍞⍬⊣⍤⌸⌷≡⊆⍪⍙⍠⊢≢"

installExitHandlers :: ProcessHandle -> IO ()
installExitHandlers dyalogHandle = do
  threadId <- myThreadId
  installHandler sigINT (interruptHandler threadId) Nothing
  installHandler sigTERM (interruptHandler threadId) Nothing
  return ()
  where
    interruptHandler threadId =
      Catch
        ( do
            -- TODO instead of terminating dyalog, send ["Exit",{"code":0}] to it
            terminateProcess dyalogHandle
            E.throwTo threadId ExitSuccess
        )

run :: IO ()
run = do
  socket <- serveTcpServer
  dyalogHandle <- spawnDyalog
  installExitHandlers dyalogHandle
  listenToConnection socket