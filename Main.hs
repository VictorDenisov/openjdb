import System.Console.Haskeline
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment (getArgs)
import Network
import GHC.IO.Handle
import Data.List (intercalate, find)
import Data.Maybe (fromMaybe)
import GHC.Word (Word16, Word32, Word8)
import Network.Socket.Internal (PortNumber(..))
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)

import Jdwp

withConnection :: String -> PortID -> (Handle -> IO ()) -> IO ()
withConnection host port f = do
    h <- connectTo host port
    hSetBinaryMode h True
    f h
    hClose h

data Flag = Version
          | Port { port :: String }
          | Host { host :: String }
            deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['v'] ["version"] (NoArg Version) "show version number"
    , Option ['p'] ["port"]    (ReqArg Port "PORT") "port number"
    , Option ['h'] ["host"]    (ReqArg Host "HOST") "host number"
    ]

cmdArgsErrMsg :: [String] -> String
cmdArgsErrMsg errors = 
    "There are errors in command args parsing:\n\n"
    ++ concat errors

isPort :: Flag -> Bool
isPort (Port _) = True
isPort _ = False

isHost :: Flag -> Bool
isHost (Host _) = True
isHost _ = False

getPort :: [Flag] -> PortID
getPort opts = PortNumber $ fromIntegral $ ((read $ port $ fromMaybe (Port "2044") (find isPort opts)) :: Int)

getHost :: [Flag] -> String
getHost opts = host $ fromMaybe (Host "localhost") (find isHost opts)

main :: IO ()
main = do
    args <- getArgs
    let (opts, unparsed, errors) = getOpt Permute options args
    if not $ null errors
    then putStr $ cmdArgsErrMsg errors
    else if Version `elem` opts
         then putStrLn "1.0"
         else if ((isPort `any` opts) && (isHost `any` opts))
              then withConnection (getHost opts) (getPort opts) mainLoop
              else putStrLn "Host and port arguments are required"

handshake :: Handle -> IO ()
handshake h = do
    putStrLn "After connect"
    hPutStr h "JDWP-Handshake"
    hFlush h
    value <- B.hGet h 14
    if value == (B8.pack "JDWP-Handshake")
    then putStrLn "Successfull handshake"
    else putStrLn "Handshake FAILED!"

mainLoop :: Handle -> IO ()
mainLoop h = do
    handshake h
    packet <- receivePacket h
    putStrLn $ show packet
    runInputT defaultSettings loop
    where 
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "(jdb) "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do
                    outputStrLn $ "Input was: " ++ input
                    liftIO . (processCommand h) $ input
                    loop

receivePacket :: Handle -> IO Packet
receivePacket h = do
    inputAvailable <- hWaitForInput h (-1)
    s <- B.hGetContents h
    let p = runGet (parsePacket (\_ -> undefined)) s
    return p

processCommand :: Handle -> String -> IO ()
processCommand h "version" = do
    B.hPut h $ runPut $ putPacket $ versionCommand 1
    putStrLn "version request sent"
    --s <- B.hGetContents h
    --putStrLn $ show s
    {-
    let p = runGet (parsePacket (\_ -> undefined)) s
    putStrLn $ show p
    -}
processCommand _ cmd = putStrLn ("Hello from processCommand " ++ cmd)

{-
main = do
    line <- B.readFile "sample/reply.txt"
    let value = runGet (parseList 2 (parsePacket (\id -> replyParser $ dataParsers (1, 1)))) line
    putStrLn $ show value
    -}
