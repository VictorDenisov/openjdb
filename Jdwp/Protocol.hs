module Jdwp.Protocol where

import Prelude hiding (length, id)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Binary (Binary(..), Get, Put)
import Data.Bits ((.&.))
import Control.Applicative ((<$>), (<*>))

------------Packet description and parsing section.
-- {{{
type PacketId = Word32
type CommandSet = Word8
type Command = Word8

data Packet = CommandPacket { length     :: Word32
                            , id         :: PacketId
                            , flags      :: Word8
                            , commandSet :: CommandSet
                            , command    :: Command
                            , dat        :: PacketData
                            }
            | ReplyPacket   { length     :: Word32
                            , id         :: PacketId
                            , flags      :: Word8
                            , errorCode  :: Word16
                            , dat        :: PacketData
                            }
              deriving Show

type ReplyDataParser = PacketId -> Get PacketData

parsePacket :: ReplyDataParser -> Get Packet
parsePacket replyDataParser = do
    l <- get
    i <- get
    f <- get
    if (f .&. 0x80) == 0
    then do
        cs <- get
        c  <- get
        d  <- commandParser $ dataParsers (cs, c)
        return (CommandPacket l i f cs c d)
    else do
        e <- get
        d <- replyDataParser i
        return (ReplyPacket l i f e d)

parseList :: Word32 -> Get a -> Get [a]
parseList 0 p = return []
parseList l p = do
    x <- p
    xs <- parseList (l - 1) p
    return (x:xs)

putPacket :: Packet -> Put
putPacket (CommandPacket l i f cs c d) = do
    put l
    put i
    put f
    put cs
    put c
    putPacketData d

putPacket (ReplyPacket l i f e d) = do
    put l
    put i
    put f
    put e
    putPacketData d
-- }}}
---------------General types section
-- {{{
type JavaByte            = Word8
type JavaInt             = Word32
type JavaLong            = Word64
type JavaString          = String
type JavaBoolean         = Bool
type JavaFieldId         = Word64
type JavaMethodId        = Word64
type JavaObjectId        = Word64
type JavaReferenceTypeId = Word64
type JavaFrameId         = Word64
type JavaThreadId        = JavaObjectId

parseByte :: Get JavaByte
parseByte = get

parseBoolean :: Get JavaBoolean
parseBoolean = (/= 0) <$> (get :: Get Word8)

parseInt :: Get JavaInt
parseInt = get

parseLong :: Get JavaLong
parseLong = get

parseString :: Get JavaString
parseString = do
    len <- (get :: Get Word32)
    list <- parseList len (get :: Get Word8)
    return $ B8.unpack $ B.pack list
-- }}}
-------PacketData parsing section---------------------
-- {{{
data PacketData = EventSet
                    { suspendPolicy :: JavaByte
                    , events        :: [Event]
                    }
                | VersionReply 
                    { description :: JavaString
                    , jdwpMajor   :: JavaInt
                    , jdwpMinor   :: JavaInt
                    , vmVersion   :: JavaString
                    , vmName      :: JavaString
                    }
                | IdSizesReply
                    { fieldIdSize         :: JavaInt
                    , methodIdSize        :: JavaInt
                    , objectIdSize        :: JavaInt
                    , referenceTypeIdSize :: JavaInt
                    , frameIdSize         :: JavaInt
                    }
                | ThreadIdPacketData
                    { tId :: JavaThreadId
                    }
                | EmptyPacketData
                  deriving Show

data Event = VmStartEvent
                { requestId :: JavaInt
                , threadId  :: JavaThreadId
                }
           | VmDeathEvent
                { requestId :: JavaInt
                }
           | NoEvent
             deriving (Show, Eq)

putPacketData :: PacketData -> Put
putPacketData (EventSet sp e) = do
    put sp
    mapM_ putEvent e
putPacketData (ThreadIdPacketData i) =
    put i
putPacketData (EmptyPacketData) = return ()

putEvent :: Event -> Put
putEvent (VmStartEvent ri ti) = do
    put ri
    put ti

parseIdSizesReply :: Get PacketData
parseIdSizesReply = IdSizesReply
                        <$> parseInt
                        <*> parseInt
                        <*> parseInt
                        <*> parseInt
                        <*> parseInt

parseVersionReply :: Get PacketData
parseVersionReply = VersionReply
                        <$> parseString
                        <*> parseInt
                        <*> parseInt
                        <*> parseString
                        <*> parseString

parseEventSet :: Get PacketData
parseEventSet = do
    sp <- parseByte
    eventCount <- parseInt
    eventList <- parseList eventCount parseEvent
    return $ EventSet sp eventList

parseEvent :: Get Event
parseEvent = do
    eventKind <- (get :: Get Word8)
    case eventKind of
        90 -> do
            requestId <- parseInt
            threadId <- parseThreadId
            return $ VmStartEvent requestId threadId
        99 -> do
            requestId <- parseInt
            return $ VmDeathEvent requestId
        _  -> return NoEvent

parseThreadId :: Get JavaThreadId
parseThreadId = get

parseEmptyData :: Get PacketData
parseEmptyData = return EmptyPacketData

data DataParser = DataParser
                { commandParser :: Get PacketData
                , replyParser   :: Get PacketData
                }

dataParsers :: (CommandSet, Command) -> DataParser
dataParsers ( 1,   1) = DataParser parseEmptyData parseVersionReply
dataParsers ( 1,   7) = DataParser parseEmptyData parseIdSizesReply
dataParsers (64, 100) = DataParser parseEventSet  parseEmptyData
dataParsers _ = undefined

-- }}}
------------Command Constructors Section
-- {{{
versionCommand :: PacketId -> Packet
versionCommand id = CommandPacket 11 id 0 1 1 EmptyPacketData

idSizesCommand :: PacketId -> Packet
idSizesCommand id = CommandPacket 11 id 0 1 7 EmptyPacketData

resumeVmCommand :: PacketId -> Packet
resumeVmCommand id = CommandPacket 11 id 0 1 9 EmptyPacketData

resumeThreadCommand :: PacketId -> JavaThreadId -> Packet
resumeThreadCommand id threadId = CommandPacket 19 id 0 11 3 (ThreadIdPacketData threadId)

-- }}}
-- vim: foldmethod=marker foldmarker={{{,}}}
