{-# LANGUAGE RecordWildCards #-}

module Lib (Flags (..), run) where

import Control.Concurrent (forkIO, myThreadId)
import qualified Control.Exception as E
import Control.Monad (forever, when)
import Data.Binary.Get as G (getInt32be, runGet)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B 
import Data.ByteString.Lazy (ByteString)
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

data Flags = Flags
  { port :: Int,
    dyalogPath :: String,
    verbose :: Bool
  }

log_ :: Bool -> String -> IO ()
log_ verbose string =
  when verbose $ putStrLn string

logBS_ :: Bool -> String -> ByteString -> IO ()
logBS_ verbose label string =
  when verbose $ do
    putStr label
    BC.putStrLn string

serveTcpServer :: Int -> IO Socket
serveTcpServer port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  let portNumber = read (show port) -- WTF
  bind sock (SockAddrInet portNumber (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 1
  return sock

spawnDyalog :: Int -> FilePath -> IO ProcessHandle
spawnDyalog port dyalogPath = do
  withFile "/dev/null" WriteMode $ \devNullHandle -> do
    (_, _, _, dyalogHandle) <-
      createProcess_
        "dyalog process"
        (proc dyalogPath dyalogArgs)
          { env =
              Just
                [ ("CLASSICMODE", "1"),
                  ("SINGLETRACE", "1"),
                  ("RIDE_INIT", "CONNECT:127.0.0.1:" ++ show port),
                  ("RIDE_SPAWNED", "1"),
                  ("APLK0", "default")
                ],
            std_in = CreatePipe,
            std_out = UseHandle devNullHandle,
            std_err = UseHandle devNullHandle
          }
    return dyalogHandle
  where
    dyalogArgs = ["+s", "-q"]

sendToDyalog :: Bool -> Handle -> String -> IO ()
sendToDyalog verbose handle string =
  let message = BB.stringUtf8 string
      messageBS = BB.toLazyByteString message
      messageLength = B.length messageBS
      everythingLength = fromIntegral (8 + messageLength) :: Int32
      everything =
        BB.int32BE everythingLength
          <> BB.stringUtf8 "RIDE"
          <> message
   in do
        log_ verbose $ "SEND: " ++ string
        BB.hPutBuilder handle everything

readFromDyalog :: Handle -> IO ByteString
readFromDyalog handle = do
  hWaitForInput handle 4
  lengthBS <- B.hGet handle 4
  let length = fromIntegral $ G.runGet G.getInt32be lengthBS
  hWaitForInput handle length
  B.hGet handle 4 -- "RIDE"
  B.hGet handle (length - 8) -- minus length minus RIDE

sendUserInput :: Bool -> Handle -> String -> IO ()
sendUserInput verbose handle string =
  sendToDyalog verbose handle wrappedString
  where
    wrappedString = "[\"Execute\",{\"text\":\"" ++ escapedString ++ "\\n\",\"trace\":0}]"
    escapedString = unpack $ replace (pack "\"") (pack "\\\"") (pack string)

listenToConnection :: Bool -> Socket -> IO ()
listenToConnection verbose sock = do
  (socket, _) <- accept sock
  handle <- socketToHandle socket ReadWriteMode
  hSetBuffering handle NoBuffering
  sendToDyalog verbose handle "SupportedProtocols=2"
  sendToDyalog verbose handle "UsingProtocol=2"
  sendToDyalog verbose handle "[\"Identify\",{\"identity\":1}]"
  sendToDyalog verbose handle "[\"Connect\",{\"remoteId\":2}]"
  sendToDyalog verbose handle "[\"GetWindowLayout\",{}]"
  -- TODO do these all in one thread:
  -- read all msgs
  -- then handle input
  -- after \n and sending the Execute thing read all msgs again
  forkIO $ handleUserInput verbose handle
  forever $ do
    message <- readFromDyalog handle
    logBS_ verbose "RECV: " message

handleUserInput :: Bool -> Handle -> IO ()
handleUserInput verbose handle = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  go False ""
  where
    go :: Bool -> String -> IO ()
    go backslashMode inputRev = do
      char <- getChar
      case char of
        '\n' -> do
          sendUserInput verbose handle (reverse inputRev)
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

run :: Flags -> IO ()
run Flags {..} = do
  socket <- serveTcpServer port
  dyalogHandle <- spawnDyalog port dyalogPath
  installExitHandlers dyalogHandle
  listenToConnection verbose socket