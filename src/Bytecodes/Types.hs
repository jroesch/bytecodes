{-# LANGUAGE GADTs, EmptyDataDecls, OverloadedStrings, RankNTypes #-}
module Bytecodes.Types where

import Data.Text (Text)
import Data.Bits
import Data.Map (Map)
import Data.Array ((!))
import qualified Data.Array as A
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as M
import qualified Bytecodes.Raw.Types as Raw
import qualified Data.ByteString as B
import Control.Applicative ((<$>))

data Version = Version Int Int deriving (Eq)

instance Show Version where
  show (Version major minor) = concat [show major, ".", show minor]

fromRClassFile :: Raw.ClassFile -> Maybe ClassFile
fromRClassFile rf = do
  this <- cfLookupClass rf $ fromIntegral $ Raw.thisClass rf
  super <- cfLookupClass rf $ fromIntegral $ Raw.superClass rf
  let  ifaces = A.elems $ Raw.interfaces rf
  interfaces <- sequence $ map (\i -> IFace <$> (cfLookupClass rf $ fromIntegral i)) ifaces
  return $ ClassFile {
    version = cfVersion rf,
    accessFlags = mkAccessFlags $ fromIntegral $ Raw.accessFlags rf,
    this = this,
    super = super,
    interfaces = interfaces,
    fields = [],
    methods = []
    -- rawClassFile = rf
  }

toRClassFile :: ClassFile -> Raw.ClassFile
toRClassFile = undefined

cfVersion :: Raw.ClassFile -> Version
cfVersion rf = Version (fromIntegral $ Raw.majorVersion rf) (fromIntegral $ Raw.minorVersion rf)

cfLookupClass :: Raw.ClassFile -> Int -> Maybe (Type Class)
cfLookupClass rf i = case cp ! (i - 1) of
                       Raw.Utf8 _ n -> Just $ Object n
                       Raw.Class n  -> cfLookupClass rf $ fromIntegral n          
                       _            -> Nothing
    where cp = Raw.constantPool rf

cfField :: Raw.FieldInfo -> Field
cfField _ = undefined 

data ClassFile = ClassFile {
  -- constant pool
  version :: Version,
  accessFlags :: [AccessFlag],
  this :: Type Class,
  super :: Type Class,
  interfaces :: [Interface],
  fields :: [Field],
  methods :: [Method]
  --attributes :: Array AttributeInfo
  --rawClassFile :: Raw.ClassFile
} deriving (Show, Eq)

data AccessFlag = Public 
                 | Final 
                 | Super 
                 | Interface 
                 | Abstract
                 | Synthetic
                 | Annotation
                 | Enum
                 deriving (Show, Eq)

data Field = Field deriving (Show, Eq)

accessFlagsMap :: Map Int AccessFlag
accessFlagsMap = M.fromList [
  (0x0001, Public), 
  (0x0010, Final),
  (0x0020, Super),
  (0x0200, Interface),
  (0x0400, Abstract),
  (0x1000, Synthetic),
  (0x2000, Annotation),
  (0x4000, Enum)]

mkAccessFlags :: Int -> [AccessFlag]
mkAccessFlags fs = 
    let bytes = [0x000F .&. fs, 0x00F0 .&. fs, 0x0F00 .&. fs, 0xF000 .&. fs] in
      let mflags = map (\x -> x `M.lookup` accessFlagsMap) bytes in
        catMaybes mflags

-- Phantom Types for Type
data Primitive
data Class 
data Array a

data Interface = IFace (Type Class) deriving (Show, Eq)
data Method = M deriving (Show, Eq)

data Type a where
  Boolean :: Type Primitive
  Char :: Type Primitive
  Byte :: Type Primitive
  Short :: Type Primitive 
  Int :: Type Primitive
  Float :: Type Primitive
  Long :: Type Primitive
  Double :: Type Primitive
  Object :: B.ByteString -> Type Class
  Array :: Type a -> Type (Array a)

instance Show (Type a) where
  show Boolean    = "Z" 
  show Char       = "C"
  show Byte       = "B" 
  show Short      = "S"
  show Int        = "I"
  show Float      = "F"
  show Long       = "J"
  show Double     = "D"
  show (Object n) = "L" ++ show n ++ ";"
  show (Array  t) = "[" ++ show t

instance Eq (Type a) where
  Boolean    == Boolean    = True
  Char       == Char       = True
  Byte       == Byte       = True
  Short      == Short      = True
  Int        == Int        = True
  Float      == Float      = True
  Long       == Long       = True
  Double     == Double     = True
  (Object n) == (Object m) = n == m
  (Array t)  == (Array u)  = t == u
  _          == _          = False

{- data MethodDecl = MethodDecl { 
  returnType :: Type,
  name :: String, 
  params :: List[ -}

