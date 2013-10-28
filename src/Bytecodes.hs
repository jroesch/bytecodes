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

type UInt1 = Word8
type UInt2 = Word16
type UInt4 = Word32

class Serialize a where
    serialize :: a -> [Word8]

data ConstantPoolInfo = 
    CClass UInt2 -- nameIndex
  | CFieldRef UInt2 UInt2 -- classIndex nameAndTypeIndex
  | CMethodRef UInt2 UInt2 -- classIndex nameAndTypeIndex
  | CInterfaceMethodRef UInt2 UInt2 -- classIndex nameAndTypeIndex
  | CString UInt2 -- stringIndex
  | CInteger UInt4 -- bytes
  | CFloat UInt4 -- bytes
  | CLong UInt4 UInt4 -- highBytes lowBytes
  | CDouble UInt4 UInt4 -- highBytes lowBytes
  | CNameAndType UInt2 UInt2 -- nameIndex descriptorIndex
  | CUtf8 UInt2 (Vector UInt1) -- length bytes
  | CMethodHandle UInt1 UInt2 -- referenceKind referenceIndex
  | CMethodType UInt2 -- descriptorIndex 
  | CInvokeDynamic UInt2 UInt2 -- bootstrapMethodAttrIndex nameAndTypeIndex
  deriving (Eq)

instance Serialize ConstantPoolInfo where
  serialize i = constant i : constant i : bytesFor i
    where
      -- Map constructors to byte constants
      constant info = case info of
        CClass _                -> 7
        CFieldRef _ _           -> 9
        CMethodRef _ _          -> 10
        CInterfaceMethodRef _ _ -> 11
        CString _               -> 8
        CInteger _              -> 3
        CFloat _                -> 4
        CLong _ _               -> 5
        CDouble _ _             -> 6
        CNameAndType _ _        -> 12
        CUtf8 _ _               -> 1
        CMethodHandle _ _       -> 15
        CMethodType _           -> 16
        CInvokeDynamic _ _      -> 18
      -- Map constructors to byte sequences
      bytesFor info = case info of
        CClass ni                 -> (toBytes ni)
        CFieldRef ci nt           -> (toBytes ci) ++ (toBytes nt)
        CMethodRef ci nt          -> (toBytes ci) ++ (toBytes nt)
        CInterfaceMethodRef ci nt -> (toBytes ci) ++ (toBytes nt)
        CString si                -> (toBytes si)
        CInteger bs               -> (toBytes bs)
        CFloat bs                 -> (toBytes bs)
        CLong hi lo               -> (toBytes hi) ++ (toBytes lo)
        CDouble hi lo             -> (toBytes hi) ++ (toBytes lo)
        CNameAndType ni di        -> (toBytes ni) ++ (toBytes di)
        CUtf8 len _               -> (toBytes len) ++ []
        CMethodHandle rk ri       -> rk : (toBytes ri)
        CMethodType di            -> toBytes di
        CInvokeDynamic b nt       -> (toBytes b) ++ (toBytes nt)

instance Show ConstantPoolInfo where
 show (CClass ni)                 = "Class #" ++ show ni
 show (CFieldRef ci nt)           = "Fieldref #" ++ (show ci) ++ "." ++ "#" ++ (show nt)
 show (CMethodRef ci nt)          = "Methodref #" ++ (show ci) ++ "." ++ "#" ++ (show nt)
 show (CInterfaceMethodRef ci nt) = undefined -- (toBytes ci) ++ (toBytes nt)
 show (CString si)                = "String #" ++ show si
 show (CInteger bs)               = "Integer " ++ show bs
 show (CFloat bs)                 = "Float " ++ show bs 
 show (CLong hi lo)               = "CLong " ++ show hi ++ show lo -- (toBytes hi) ++ (toBytes lo)
 show (CDouble hi lo)             = "CDouble " ++ show hi ++ show lo -- (toBytes hi) ++ (toBytes lo)
 show (CNameAndType ni di)        = printf "NameAndType #%d:%d" ni di
 show (CUtf8 len bs)              = (++) "Utf8 " $ map (chr . fromIntegral) $ V.toList bs 
 show (CMethodHandle rk ri)       = undefined -- rk : (toBytes ri)
 show (CMethodType di)            = undefined -- toBytes di
 show (CInvokeDynamic b nt)       = undefined -- (toBytes b) ++ (toBytes nt)


type AccessFlag = ()
type MethodInfo = ()

toBytes :: forall a. (Storable a, Integral a, Num a) => a -> [Word8]
toBytes x = step (sizeOf x) x where
  step 1 i = [fromInteger $ toInteger i]
  step n i = let 
    y  = fromInteger $ toInteger i `shiftR` (sum $ replicate (n - 1) 8)
    ys = step (n - 1) i in y : ys

class (Storable a, Integral a, Num a) => FromByteString a where
  fromByteString :: B.ByteString -> (a, B.ByteString)
  fromByteString bs = let
    len = sizeOf (undefined :: a) in
    (fromInteger $ readI len bs, B.drop len bs)
    where
      readI 1 bs = toInteger $ B.head bs
      readI n bs = let a = toInteger $ B.head bs in
        a `shift` (sum $ replicate (n - 1)  8) .|. (readI (n - 1) $ B.tail bs)

instance FromByteString UInt1
instance FromByteString UInt2
instance FromByteString UInt4

data ClassFile = ClassFile { 
  magic :: UInt4, 
  minorVersion :: UInt2,
  majorVersion :: UInt2,
  constantPoolCount :: UInt2,
  constantPool :: Vector ConstantPoolInfo,
  accessFlags :: UInt2,
  thisClass :: UInt2,
  superClass :: UInt2,
  interfacesCount :: UInt2,
  interfaces :: Vector UInt2,
  fieldsCount :: UInt2,
  fields :: Vector FieldInfo,
  methodsCount :: UInt2,
  methods :: Vector MethodInfo,
  attributesCount :: UInt2,
  attributes :: Vector AttributeInfo
} deriving (Show, Eq)

toUInt2 :: Word8 -> Word8 -> UInt2
toUInt2 a b = fromInteger $
  (toInteger a) `shift` 8  .|.
  (toInteger b)

toUInt4 :: Word8 -> Word8 -> Word8 -> Word8 -> UInt4
toUInt4 a b c d = fromInteger $
  (toInteger a) `shift` 24 .|.
  (toInteger b) `shift` 16 .|.
  (toInteger c) `shift` 8  .|.
  (toInteger d)

type ClassBuffer = State B.ByteString

lFromByteString :: FromByteString a => ClassBuffer a
lFromByteString = do
  bs <- get
  let (v, r) = fromByteString bs
  put r
  return v

uint1 :: ClassBuffer UInt1
uint1 = lFromByteString

uint2 :: ClassBuffer UInt2
uint2 = lFromByteString

uint4 :: ClassBuffer UInt4
uint4 = lFromByteString

data FieldInfo = FieldInfo UInt2 UInt2 UInt2 UInt2 (Vector AttributeInfo) deriving (Show, Eq)
type AttributeInfo = ()

parseFieldInfo :: ClassBuffer FieldInfo
parseFieldInfo = do
    af <- uint2 -- accessFlags
    ni <- uint2 -- nameIndex
    di <- uint2 -- descriptorIndex
    ac <- uint2 -- attributesCount
    ai <- return [] -- attributes
    return $ FieldInfo af ni di ac ai

parseConstantPoolInfo :: ClassBuffer ConstantPoolInfo
parseConstantPoolInfo = do
  tag <- uint1 -- tag
  case tag of  -- info
    7 -> do
      nameI <- uint2
      return $ CClass nameI
    9 -> do
      classI <- uint2
      nameTI <- uint2
      return $ CFieldRef classI nameTI           
    10 -> do
      --tag    <- uint1
      classI <- uint2
      nameTI <- uint2
      return $ CMethodRef classI nameTI         
    11 -> do
      classI <- uint2
      nameTI <- uint2
      return $ CInterfaceMethodRef classI nameTI    
    8 -> do
      idx <- uint2
      return $ CString idx              
    3 -> do
      bytes <- uint4
      return $ CInteger bytes      
    4 -> do
      bytes <- uint4
      return $ CFloat bytes                 
    5 -> do
      hi <- uint4
      lo <- uint4
      return $ CLong hi lo            
    6 -> do
      hi <- uint4
      lo <- uint4
      return $ CDouble hi lo           
    12 -> do
      nameI <- uint2
      descpI <- uint2
      return $ CNameAndType nameI descpI       
    1 -> do
      len <- uint2
      bytes <- sequence [ uint1 | x <- [1..len]]
      return $ CUtf8 len $ V.fromList bytes    
    15 -> return $ CMethodHandle undefined undefined      
    16 -> return $ CMethodType undefined        
    18 -> return $ CInvokeDynamic undefined undefined
    _  -> error $ "unknow constant: " ++ (show tag)

parseClassFile :: B.ByteString -> ClassFile
parseClassFile bs = fst $ (flip runState) bs $ do
  magic <- uint4
  minorVersion <- uint2
  majorVersion <- uint2
  constantPoolCount <- uint2 
  constantPool <- do
    cp <- sequence $ replicate (fromIntegral $ constantPoolCount - 1) (parseConstantPoolInfo)
    return $ V.fromList cp -- constant pool
  accessFlags <- uint2 -- uint2
  thisClass <- uint2
  superClass <- uint2 
  interfacesCount <- uint2
  interfaces <- do
    is <- sequence $ replicate (fromIntegral interfacesCount) uint2
    return $ V.fromList is -- interfaces
  fieldsCount <- uint2
  fields <- return $ V.fromList $ [()] -- field_info
  methodsCount <- uint2
  methods <- return $ V.fromList $ [()] -- method_info
  attributesCount <- uint2
  attributes <- return $ V.fromList $ [()] -- attribute_info
  return $ (ClassFile
    magic
    minorVersion
    majorVersion
    constantPoolCount
    constantPool
    accessFlags
    thisClass
    superClass
    interfacesCount
    interfaces
    fieldsCount
    fields
    methodsCount
    methods
    attributesCount
    attributes)

blah f = do
  handle <- openFile f ReadMode
  file <- B.hGetContents handle
  let rawBytes = file
      classFile = parseClassFile rawBytes in
        return classFile

test = blah "/Users/jroesch/Desktop/Test.class"
