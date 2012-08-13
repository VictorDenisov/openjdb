module Jdwp.Configuration where

import Control.Monad.State (StateT(..),MonadState(..))
import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map as M
import Data.Word (Word8, Word16, Word32, Word64)

import Jdwp.Protocol

data Configuration = Configuration
    { idSizes        :: IdSizes
    , commandCounter :: Word32
    , replyParsers   :: M.Map PacketId ReplyDataParser
    }

instance Eq Configuration where
    a == b = 
             idSizes a == idSizes b
          && commandCounter a == commandCounter b

instance Show Configuration where
    show c = (show $ idSizes c) ++ " " ++ (show $ commandCounter c)

data IdSizes = IdSizes
    { fieldIdSizeConf         :: JavaInt
    , methodIdSizeConf        :: JavaInt
    , objectIdSizeConf        :: JavaInt
    , referenceTypeIdSizeConf :: JavaInt
    , frameIdSizeConf         :: JavaInt
    } deriving (Eq, Show)

type ConfigT = StateT Configuration

type Config  = ConfigT Identity

initConf = Configuration (IdSizes 0 0 0 0 0) 0 M.empty

runConfT = runStateT

runConf s v = runIdentity $ (runStateT s) v

incCmdCounter :: Monad m => ConfigT m ()
incCmdCounter = do
    s <- get
    put $ s { commandCounter = (commandCounter s) + 1 }

setIdSizes :: Monad m => IdSizes -> ConfigT m ()
setIdSizes iss = do
    s <- get
    put $ s { idSizes = iss }
