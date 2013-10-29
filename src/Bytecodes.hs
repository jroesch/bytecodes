{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Bytecodes where

import System.IO
import Data.Word
import Data.Bits
import Control.Monad.Trans.State
import Control.Monad
import qualified Data.ByteString.Internal as BI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Foreign.Storable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Char
import Text.Printf
import Debug.Trace
import Bytecodes.Types
import Data.Serialize

blah f = do
  handle <- openFile f ReadMode
  file <- B.hGetContents handle
  let rawBytes = file
      classFile = decode rawBytes in
        return classFile

test :: IO (Either String ClassFile)
test = blah "/Users/jroesch/Desktop/Test.class"
