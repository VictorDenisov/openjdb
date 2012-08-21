module Jdwp.Configuration where

import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map as M
import Data.Word (Word8, Word16, Word32, Word64)
import Control.Monad (liftM)

import Jdwp.Protocol

data Configuration = Configuration
    { idSizes        :: IdSizes
    , packetIdCounter :: PacketId
    , replyParsers   :: M.Map PacketId ReplyDataParser
    }

instance Eq Configuration where
    a == b = 
             idSizes a == idSizes b
          && packetIdCounter a == packetIdCounter b

instance Show Configuration where
    show c = (show $ idSizes c) ++ " " ++ (show $ packetIdCounter c)

data IdSizes = IdSizes
    { fieldIdSizeConf         :: JavaInt
    , methodIdSizeConf        :: JavaInt
    , objectIdSizeConf        :: JavaInt
    , referenceTypeIdSizeConf :: JavaInt
    , frameIdSizeConf         :: JavaInt
    } deriving (Eq, Show)

type ConfT = StateT Configuration

type Conf  = ConfT Identity

initConf = Configuration (IdSizes 0 0 0 0 0) 0 M.empty

runConfT = runStateT

evalConfT :: Monad m => ConfT m a -> Configuration -> m a
evalConfT = evalStateT

runConf s v = runIdentity $ (runStateT s) v

evalConf s v = runIdentity $ (evalConfT s) v

getPacketIdCounter :: Monad m => ConfT m PacketId
getPacketIdCounter = liftM packetIdCounter get

incPacketIdCounter :: Monad m => ConfT m ()
incPacketIdCounter = do
    s <- get
    put $ s { packetIdCounter = (packetIdCounter s) + 1 }

setIdSizes :: Monad m => IdSizes -> ConfT m ()
setIdSizes iss = do
    s <- get
    put $ s { idSizes = iss }
