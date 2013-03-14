module Main where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPutStr, BufferMode(..), Handle)

import Network

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = getPort args
    database <- atomically $ newTVar $ fromList [("__version__", version)]
    sock <- listenOn $ PortNumber $ fromIntegral port
    putStrLn $ "Listening on localhost:" ++ (show port)
    sockHandler sock database

getPort :: [String] -> Int
getPort (x:_) = read x :: Int
getPort [] = 7777

crlf :: String
crlf = "\r\n"

sockHandler :: Socket -> (TVar DB) -> IO ()
sockHandler sock db = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- forkIO $ commandProcessor handle db
    sockHandler sock db

getCommand :: Handle -> String -> (TVar DB) -> IO ()
getCommand handle cmd db = do
    m <- atomRead db
    value <- getValue m cmd
    hPutStr handle $ concat ["$", valLength value, crlf, value, crlf]
        where
            valLength = show . length

setCommand :: Handle -> String -> String -> (TVar DB) -> IO ()
setCommand handle key value db = do
    updateValue (insert key value) db
    hPutStr handle $ concat ["+OK", crlf]

commandProcessor :: Handle -> (TVar DB) -> IO ()
commandProcessor handle db = do
    line <- hGetLine handle
    let cmd = words line
    case cmd of
        "*2":_ -> do
            _   <- hGetLine handle -- argSize
            _   <- hGetLine handle -- arg
            _   <- hGetLine handle -- keySize
            key <- hGetLine handle
            getCommand handle key db

        "*3":_ -> do
            _     <- hGetLine handle -- argSize
            _     <- hGetLine handle -- arg
            _     <- hGetLine handle -- keySize
            key   <- hGetLine handle
            _     <- hGetLine handle -- valueSize
            value <- hGetLine handle
            setCommand  handle key value db

        _  -> do hPutStrLn handle "Unknown command"
    commandProcessor handle db
