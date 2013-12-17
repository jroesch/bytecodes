{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, DeriveGeneric, DefaultSignatures #-}
module Bytecodes.Raw.Types where

import Control.Monad
import Data.Serialize
import Data.Word
import Data.Char
import Text.Printf
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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


-- alias for consistency 
uint4 = getWord32be
uint2 = getWord16be
uint1 = getWord8

puint4 = putWord32be
puint2 = putWord16be
puint1 = putWord8

fromList xs = A.listArray (0, length xs - 1) xs

getArrayOfSize :: forall e i. (Serialize e, Integral i) => i -> Get (Array e)
getArrayOfSize i = do
  rs <- replicateM (fromIntegral i) (get :: Get e)
  return $ fromList rs

instance Serialize ClassFile where
  get = do
    magic <- uint4
    when (magic /= 0xCAFEBABE) $ 
      error "Magic number fail!"
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

  put cf = do
    puint4 0xCAFEBABE
    put $ minorVersion cf
    put $ majorVersion cf
    put $ constantPoolCount cf + 1
    put $ constantPool cf
    put $ accessFlags cf
    put $ thisClass cf
    put $ superClass cf
    put $ (interfacesCount cf :: UInt2) -- interfacesCount cf
    put $ interfaces cf
    put $ (0 :: UInt2) -- fieldsCount cf
    -- put $ fields cf    
    put $ (0 :: UInt2) -- methodsCount cf
    -- put $ methods cf -- <- getArrayOfSize methodsCount -- method_info
    put $ (0 :: UInt2) -- attributesCount cf -- <- uint2
    -- put $ attributes cf -- <- getArrayOfSize attributesCount -- attribute_info


data ConstantPoolInfo = 
    Class UInt2 -- nameIndex
  | FieldRef UInt2 UInt2 -- classIndex nameAndTypeIndex
  | MethodRef UInt2 UInt2 -- classIndex nameAndTypeIndex
  | InterfaceMethodRef UInt2 UInt2 -- classIndex nameAndTypeIndex
  | String UInt2 -- stringIndex
  | Integer UInt4 -- bytes
  | Float UInt4 -- bytes
  | Long UInt4 UInt4 -- highBytes lowBytes
  | Double UInt4 UInt4 -- highBytes lowBytes
  | NameAndType UInt2 UInt2 -- nameIndex descriptorIndex
  | Utf8 UInt2 B.ByteString -- length bytes
  | MethodHandle UInt1 UInt2 -- referenceKind referenceIndex
  | MethodType UInt2 -- descriptorIndex 
  | InvokeDynamic UInt2 UInt2 -- bootstrapMethodAttrIndex nameAndTypeIndex
  deriving (Eq)

instance Show ConstantPoolInfo where
  show (Class ni)                 = "Class #" ++ show ni
  show (FieldRef ci nt)           = "Fieldref #" ++ (show ci) ++ "." ++ "#" ++ (show nt)
  show (MethodRef ci nt)          = "Methodref #" ++ (show ci) ++ "." ++ "#" ++ (show nt)
  show (InterfaceMethodRef ci nt) = undefined -- (toBytes ci) ++ (toBytes nt)
  show (String si)                = "String #" ++ show si
  show (Integer bs)               = "Integer " ++ show bs
  show (Float bs)                 = "Float " ++ show bs 
  show (Long hi lo)               = "CLong " ++ show hi ++ show lo -- (toBytes hi) ++ (toBytes lo)
  show (Double hi lo)             = "CDouble " ++ show hi ++ show lo -- (toBytes hi) ++ (toBytes lo)
  show (NameAndType ni di)        = printf "NameAndType #%d:%d" ni di
  show (Utf8 len bs)              = (++) "Utf8 " $ BC.unpack bs 
  show (MethodHandle rk ri)       = undefined -- rk : (toBytes ri)
  show (MethodType di)            = undefined -- toBytes di
  show (InvokeDynamic b nt)       = undefined -- (toBytes b) ++ (toBytes nt)

instance Serialize ConstantPoolInfo where
  get = do
    tag <- uint1 -- tag
    case tag of  -- info
      7 -> do
        nameI <- uint2
        return $ Class nameI
      9 -> do
        classI <- uint2
        nameTI <- uint2
        return $ FieldRef classI nameTI           
      10 -> do
        classI <- uint2
        nameTI <- uint2
        return $ MethodRef classI nameTI         
      11 -> do
        classI <- uint2
        nameTI <- uint2
        return $ InterfaceMethodRef classI nameTI    
      8 -> do
        idx <- uint2
        return $ String idx              
      3 -> do
        bytes <- uint4
        return $ Integer bytes      
      4 -> do
        bytes <- uint4
        return $ Float bytes                 
      5 -> do
        hi <- uint4
        lo <- uint4
        return $ Long hi lo            
      6 -> do
        hi <- uint4
        lo <- uint4
        return $ Double hi lo           
      12 -> do
        nameI <- uint2
        descpI <- uint2
        return $ NameAndType nameI descpI       
      1 -> do
        len <- uint2
        bytes <- getByteString $ fromIntegral len
        return $ Utf8 len bytes -- should just be text 
      15 -> do
        referenceKind <- uint1
        referenceIndex <- uint2
        return $ MethodHandle referenceKind referenceIndex    
      16 -> uint2 >>= return . MethodType       
      18 -> do
        bmai <- uint2
        nati <- uint2
        return $ InvokeDynamic bmai nati
      _  -> error $ "ConstantPoolInfo decoding unknown constant: " ++ (show tag)

  put info = case info of
    Class i -> do
      putWord8 7
      putWord16be i
    FieldRef _ _ -> do
      putWord8 9
    MethodRef _ _ -> do
      putWord8 10
    InterfaceMethodRef _ _ -> do
      putWord8 11
    String _ -> do
      putWord8 8
    Integer _ -> do
      putWord8 3
    Float _  -> do
      putWord8 4
    Long _ _ -> do
      putWord8 5
    Double _ _ -> do
      putWord8 6
    NameAndType _ _ -> do
      putWord8 12
    Utf8 _ _ -> do
      putWord8 1
    MethodHandle _ _ -> do
      putWord8 15
    MethodType _ -> do
      putWord8 16
    InvokeDynamic _ _ -> do
      putWord8 18

data FieldInfo = FieldInfo UInt2 UInt2 UInt2 UInt2 (Array AttributeInfo) deriving (Show, Eq, Generic)

instance Serialize FieldInfo where
  get = do
    accessFlags <- uint2
    nameIndex <- uint2
    dindex <- uint2
    attrCount <- uint2
    attrInfo <- getArrayOfSize attrCount
    return $ FieldInfo accessFlags nameIndex dindex attrCount attrInfo

  {- put = undefined -}

data MethodInfo = MethodInfo UInt2 UInt2 UInt2 UInt2 (Array AttributeInfo) deriving (Show, Eq, Generic)

instance Serialize MethodInfo where
  get = do
    accessFlags <- uint2
    nameIndex <- uint2
    dindex <- uint2
    attrCount <- uint2
    attrInfo <- (getArrayOfSize attrCount) :: Get (Array AttributeInfo)
    return $ MethodInfo accessFlags nameIndex dindex attrCount attrInfo

 {- put = undefined -}

data AttributeInfo = AttributeInfo UInt2 UInt4 (Array UInt1) deriving (Show, Eq, Generic)

instance Serialize AttributeInfo where
  get = do
    ani <- uint2
    alen <- uint4
    info <- getArrayOfSize alen
    return $ AttributeInfo ani alen info

  {- put = undefined -}
