{-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}
module Bytecodes.Types where

import Control.Monad
import Data.Serialize
import Data.Word
import Data.Char
import Text.Printf
import qualified Data.Array as A

type Array e = A.Array Int e

type UInt1 = Word8
type UInt2 = Word16
type UInt4 = Word32

data ClassFile = ClassFile { 
  magic :: UInt4, 
  minorVersion :: UInt2,
  majorVersion :: UInt2,
  constantPoolCount :: UInt2,
  constantPool :: Array ConstantPoolInfo,
  accessFlags :: UInt2,
  thisClass :: UInt2,
  superClass :: UInt2,
  interfacesCount :: UInt2,
  interfaces :: Array UInt2,
  fieldsCount :: UInt2,
  fields :: Array FieldInfo,
  methodsCount :: UInt2,
  methods :: Array MethodInfo,
  attributesCount :: UInt2,
  attributes :: Array AttributeInfo
} deriving (Show, Eq)

uint4 = getWord32be
uint2 = getWord16be
uint1 = getWord8

fromList xs = A.listArray (0, length xs - 1) xs

getArrayOfSize :: forall e i. (Serialize e, Integral i) => i -> Get (Array e)
getArrayOfSize i = do
  rs <- replicateM (fromIntegral i) (get :: Get e)
  return $ fromList rs

instance Serialize ClassFile where
  get = do
    magic <- uint4
    minorVersion <- uint2
    majorVersion <- uint2
    constantPoolCount <- uint2 
    constantPool <- getArrayOfSize (constantPoolCount - 1) -- constant pool
    accessFlags <- uint2 -- uint2
    thisClass <- uint2
    superClass <- uint2 
    interfacesCount <- uint2
    interfaces <- getArrayOfSize interfacesCount
    fieldsCount <- uint2
    fields <- getArrayOfSize fieldsCount -- field_info
    methodsCount <- uint2
    methods <- getArrayOfSize methodsCount -- method_info
    attributesCount <- uint2
    attributes <- getArrayOfSize attributesCount -- attribute_info
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

  put = undefined 

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
  | CUtf8 UInt2 (Array UInt1) -- length bytes
  | CMethodHandle UInt1 UInt2 -- referenceKind referenceIndex
  | CMethodType UInt2 -- descriptorIndex 
  | CInvokeDynamic UInt2 UInt2 -- bootstrapMethodAttrIndex nameAndTypeIndex
  deriving (Eq)

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
  show (CUtf8 len bs)              = (++) "Utf8 " $ map (chr . fromIntegral) $ A.elems bs 
  show (CMethodHandle rk ri)       = undefined -- rk : (toBytes ri)
  show (CMethodType di)            = undefined -- toBytes di
  show (CInvokeDynamic b nt)       = undefined -- (toBytes b) ++ (toBytes nt)

instance Serialize ConstantPoolInfo where
  get = do
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
        bytes <- replicateM (fromIntegral len) uint1
        return $ CUtf8 len $ fromList bytes -- should just be text 
      15 -> do
        referenceKind <- uint1
        referenceIndex <- uint2
        return $ CMethodHandle referenceKind referenceIndex    
      16 -> uint2 >>= return . CMethodType       
      18 -> do
        bmai <- uint2
        nati <- uint2
        return $ CInvokeDynamic bmai nati
      _  -> error $ "ConstantPoolInfo decoding unknown constant: " ++ (show tag)

  put info = case info of
    CClass i -> do
      putWord8 7
      putWord16be i
    CFieldRef _ _ -> do
      putWord8 9
    CMethodRef _ _ -> do
      putWord8 10
    CInterfaceMethodRef _ _ -> do
      putWord8 11
    CString _ -> do
      putWord8 8
    CInteger _ -> do
      putWord8 3
    CFloat _  -> do
      putWord8 4
    CLong _ _ -> do
      putWord8 5
    CDouble _ _ -> do
      putWord8 6
    CNameAndType _ _ -> do
      putWord8 12
    CUtf8 _ _ -> do
      putWord8 1
    CMethodHandle _ _ -> do
      putWord8 15
    CMethodType _ -> do
      putWord8 16
    CInvokeDynamic _ _ -> do
      putWord8 18

data FieldInfo = FieldInfo UInt2 UInt2 UInt2 UInt2 (Array AttributeInfo) deriving (Show, Eq)

instance Serialize FieldInfo where
  get = do
    accessFlags <- uint2
    nameIndex <- uint2
    dindex <- uint2
    attrCount <- uint2
    attrInfo <- getArrayOfSize attrCount
    return $ FieldInfo accessFlags nameIndex dindex attrCount attrInfo

  put = undefined

data MethodInfo = MethodInfo UInt2 UInt2 UInt2 UInt2 (Array AttributeInfo) deriving (Show, Eq)

instance Serialize MethodInfo where
  get = do
    accessFlags <- uint2
    nameIndex <- uint2
    dindex <- uint2
    attrCount <- uint2
    attrInfo <- (getArrayOfSize attrCount) :: Get (Array AttributeInfo)
    return $ MethodInfo accessFlags nameIndex dindex attrCount attrInfo

  put = undefined

data AttributeInfo = AttributeInfo UInt2 UInt4 (Array UInt1) deriving (Show, Eq)

instance Serialize AttributeInfo where
  get = do
    ani <- uint2
    alen <- uint4
    info <- getArrayOfSize alen
    return $ AttributeInfo ani alen info

  put = undefined 
