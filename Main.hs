import System.Console.Haskeline
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment (getArgs)
import Network
import GHC.IO.Handle
import Data.List (intercalate, find)
import Data.Maybe (fromMaybe)
import GHC.Word (Word16, Word32, Word8)
import Network.Socket.Internal (PortNumber(..))
import Control.Monad.Trans (liftIO, lift)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Binary.Get (runGet, Get)
import Data.Binary.Put (runPut)
import Data.Binary (get)

import Jdwp.Protocol
import Jdwp.Configuration

openConnection :: String -> PortID -> IO Handle
openConnection host port = do
    h <- connectTo host port
    hSetBinaryMode h True
    return h

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
              then bracket (openConnection (getHost opts) (getPort opts)) hClose mainLoop
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
    packet <- waitEvent h -- receive VmStartEvent
    putStrLn $ show packet
    runInputT defaultSettings $ evalConfT loop initConf
    where 
        loop :: ConfT (InputT IO) ()
        loop = do
            minput <- lift $ getInputLine "(jdb) "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do
                    lift $ outputStrLn $ "Input was: " ++ input
                    cntr <- getPacketIdCounter
                    incPacketIdCounter
                    processCommand h cntr input
                    loop

processCommand :: Handle -> PacketId -> String -> ConfT (InputT IO) ()
processCommand h cntr "version" = do
    liftIO $ sendPacket h $ versionCommand cntr
    liftIO $ putStrLn "version request sent"
    p <- liftIO $ waitReply h $ \_ -> parseVersionReply
    liftIO $ putStrLn $ show p

processCommand h cntr "resume" = do
    liftIO $ sendPacket h $ resumeVmCommand cntr
    r <- liftIO $ waitReply h $ \_ -> parseEmptyData
    liftIO $ putStrLn $ show r
    e <- liftIO $ waitEvent h
    liftIO $ putStrLn $ show e

processCommand h cntr "idsizes" = do
    liftIO $ sendPacket h $ idSizesCommand cntr
    r <- liftIO $ waitReply h $ \_ -> parseIdSizesReply
    liftIO $ putStrLn $ show r

processCommand _ _ cmd = liftIO $
    putStrLn ("Hello from processCommand " ++ cmd)

receivePacket :: Handle -> ReplyDataParser -> IO Packet
receivePacket h f = do
    inputAvailable <- hWaitForInput h (-1)
    if inputAvailable
    then do putStrLn "Input is available"
            lengthString <- B.hGet h 4
            let length = (fromIntegral $ runGet (parseInt) lengthString) - 4
            putStrLn $ show length
            reminder <- B.hGet h length
            let p = runGet (parsePacket f) (lengthString `B.append` reminder)
            return p
    else error "No input available where expected"

waitReply :: Handle -> ReplyDataParser -> IO Packet
waitReply h f = do
    packet <- receivePacket h f
    case packet of
        CommandPacket _ _ _ _ _ _ -> error "reply expected, but command received"
        {- Normally here some queue should be implemented, but currectly for brevity
         - we assume that we never get event before reply.
         -}
        ReplyPacket _ _ _ _ _ -> return packet

waitEvent :: Handle -> IO Packet
waitEvent h = do
    packet <- receivePacket h $ \_ -> error "ReplyDataParser is invoked where only command parsing is expected"
    case packet of
        CommandPacket _ _ _ _ _ _ -> return packet
        ReplyPacket _ _ _ _ _ -> error "CommandPacket is expected, but reply packet received"

sendPacket :: Handle -> Packet -> IO ()
sendPacket h p = do
    B.hPut h $ runPut $ putPacket p
    hFlush h
