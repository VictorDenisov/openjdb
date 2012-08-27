module Jdwp.Protocol where

import Prelude hiding (length, id)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Binary (Binary(..), Get, Put)
import Data.Bits ((.&.))
import Data.List (find)
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

parseReferenceTypeId :: Get JavaReferenceTypeId
parseReferenceTypeId = get
-- }}}
-------PacketData parsing section---------------------
-- {{{
data PacketData = EventSet
                    { suspendPolicy :: SuspendPolicy
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
                | EventRequestSetPacketData
                    { eventKind :: EventKind
                    , suspendPolicy :: SuspendPolicy
                    }
                | EventRequestSetReply
                    { requestIdReply :: JavaInt
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
           | ClassPrepareEvent
                { requestId   :: JavaInt
                , threadId    :: JavaThreadId
                , refTypeTag  :: TypeTag
                , typeId      :: JavaReferenceTypeId
                , signature   :: JavaString
                , classStatus :: ClassStatus
                }
           | NoEvent
             deriving (Show, Eq)

data EventKind = VmDisconnected
               | VmStart
               | ThreadDeath
               | SingleStep
               | Breakpoint
               | FramePop
               | Exception
               | UserDefined
               | ThreadStart
               | ThreadEnd
               | ClassPrepare
               | ClassUnload
               | ClassLoad
               | FieldAccess
               | FieldModification
               | ExceptionCatch
               | MethodEntry
               | MethodExit
               | VmInit
               | VmDeath
                 deriving (Eq, Show)

data SuspendPolicy = None
                   | EventThread
                   | All
                     deriving (Eq, Show)

data TypeTag = Class
             | Interface
             | Array
               deriving (Eq, Show)

newtype ClassStatus = ClassStatus JavaInt
                      deriving (Eq, Show)

eventKindNumberList :: [(JavaByte, EventKind)]
eventKindNumberList = [ (  1, SingleStep)
                      , (  2, Breakpoint)
                      , (  3, FramePop)
                      , (  4, Exception)
                      , (  5, UserDefined)
                      , (  6, ThreadStart)
                      , (  7, ThreadEnd)
                      , (  8, ClassPrepare)
                      , (  9, ClassUnload)
                      , ( 10, ClassLoad)
                      , ( 20, FieldAccess)
                      , ( 21, FieldModification)
                      , ( 30, ExceptionCatch)
                      , ( 40, MethodEntry)
                      , ( 41, MethodExit)
                      , ( 90, VmInit)
                      , ( 99, VmDeath)
                      , (100, VmDisconnected)
                      ]

eventKindFromNumber :: JavaByte -> EventKind
eventKindFromNumber n = case find ((== n) . fst) eventKindNumberList of
                            Just (_, v)  -> v
                            Nothing -> error $ "Number " ++ (show n) ++ " doesn't match any eventKind"

numberFromEventKind :: EventKind -> JavaByte
numberFromEventKind e = case find ((== e) . snd) eventKindNumberList of
                            Just (n, _) -> n
                            Nothing     -> error $ "eventKindNumberList doesn't have EventKind " ++ (show e)

suspendPolicyFromNumber :: JavaByte -> SuspendPolicy
suspendPolicyFromNumber 0 = None
suspendPolicyFromNumber 1 = EventThread
suspendPolicyFromNumber 2 = All

tagTypeFromNumber :: JavaByte -> TypeTag
tagTypeFromNumber 1 = Class
tagTypeFromNumber 2 = Interface
tagTypeFromNumber 3 = Array

putSuspendPolicy :: SuspendPolicy -> Put
putSuspendPolicy None        = put (0 :: JavaByte)
putSuspendPolicy EventThread = put (1 :: JavaByte)
putSuspendPolicy All         = put (2 :: JavaByte)

putClassStatus :: ClassStatus -> Put
putClassStatus (ClassStatus v) = put v

putEventKind :: EventKind -> Put
putEventKind e = put $ numberFromEventKind e

parseEventKind :: Get EventKind
parseEventKind = eventKindFromNumber <$> (get :: Get JavaByte)

putTypeTag :: TypeTag -> Put
putTypeTag Class     = put (1 :: JavaByte)
putTypeTag Interface = put (2 :: JavaByte)
putTypeTag Array     = put (3 :: JavaByte)

putPacketData :: PacketData -> Put
putPacketData (EventSet sp e) = do
    putSuspendPolicy sp
    mapM_ putEvent e
putPacketData (ThreadIdPacketData i) =
    put i
putPacketData (EventRequestSetPacketData ek sp) = do
    putEventKind ek
    putSuspendPolicy sp
    put (0 :: JavaInt)
putPacketData (EmptyPacketData) = return ()

putEvent :: Event -> Put
putEvent (VmStartEvent ri ti) = do
    put ri
    put ti

parseSuspendPolicy :: Get SuspendPolicy
parseSuspendPolicy = suspendPolicyFromNumber <$> (get :: Get JavaByte)

parseTypeTag :: Get TypeTag
parseTypeTag = tagTypeFromNumber <$> (get :: Get JavaByte)

parseClassStatus :: Get ClassStatus
parseClassStatus = ClassStatus <$> get

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

parseEventSetRequestReply :: Get PacketData
parseEventSetRequestReply = EventRequestSetReply
                        <$> parseInt

parseEventSet :: Get PacketData
parseEventSet = do
    sp <- parseSuspendPolicy
    eventCount <- parseInt
    eventList <- parseList eventCount parseEvent
    return $ EventSet sp eventList

parseEvent :: Get Event
parseEvent = do
    eventKind <- parseEventKind
    case eventKind of
        ClassPrepare -> ClassPrepareEvent
                            <$> parseInt
                            <*> parseThreadId
                            <*> parseTypeTag
                            <*> parseReferenceTypeId
                            <*> parseString
                            <*> parseClassStatus
        VmInit  -> VmStartEvent <$> parseInt <*> parseThreadId
        VmDeath -> VmDeathEvent <$> parseInt
        _       -> return NoEvent

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
dataParsers (15,   1) = DataParser parseEmptyData parseEventSetRequestReply
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

eventSetRequest :: PacketId -> EventKind -> SuspendPolicy -> Packet
eventSetRequest id ek sp = CommandPacket 17 id 0 15 1 $ EventRequestSetPacketData ek sp

-- }}}
-- vim: foldmethod=marker foldmarker={{{,}}}
