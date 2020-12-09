module Lib (run) where

import System.Exit (ExitCode(ExitSuccess))
import Control.Concurrent (myThreadId, forkIO)
import Control.Monad (forever)
import Data.Binary.Get as G (getInt32be, runGet)
import qualified Control.Exception as E
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Int (Int32)
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
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    IOMode (ReadWriteMode, WriteMode),
    hSetBuffering,
    hWaitForInput,
    withFile,
  )
import System.Posix.Signals
    ( installHandler, sigINT, sigTERM, Handler(Catch) )
import System.Process
  ( CreateProcess (env, std_err, std_in, std_out),
    StdStream (CreatePipe, UseHandle),
    ProcessHandle,
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
    message <- readFromDyalog handle
    putStrLn $ "Received: " ++ message

handleUserInput :: Handle -> IO ()
handleUserInput handle = do
  -- TODO
  return ()




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