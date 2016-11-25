{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.FunctionParameterAttribute where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Adds additional information to the return type and to each parameter of a function.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_function_parameter_attribute_a_function_parameter_attribute Source for Function Parameter Attribute>
data FunctionParameterAttribute =
    -- | Value should be zero extended if needed.
    Zext
    -- | Value should be sign extended if needed.
  | Sext
    -- | This indicates that the pointer parameter should really be passed by value to the function. Only valid for pointer parameters (not for ret value).
  | ByVal
    -- | Indicates that the pointer parameter specifies the address of a structure that is the return value of the function in the source program. Only applicable to the first parameter which must be a pointer parameters.
  | Sret
    -- | Indicates that the memory pointed to by a pointer parameter is not accessed via pointer values which are not derived from this pointer parameter. Only valid for pointer parameters. Not valid on return values.
  | NoAlias
    -- | The callee does not make a copy of the pointer parameter into a location that is accessible after returning from the callee. Only valid for pointer parameters. Not valid on return values.
  | NoCapture
    -- | Can only read the memory pointed to by a pointer parameter. Only valid for pointer parameters. Not valid on return values.
  | NoWrite
    -- | Cannot dereference the memory pointed to by a pointer parameter. Only valid for pointer parameters. Not valid on return values.
  | NoReadWrite
  deriving(Read, Show, Eq, Ord)

instance SpirEnum FunctionParameterAttribute Word32 where
  toWord Zext = 0
  toWord Sext = 1
  toWord ByVal = 2
  toWord Sret = 3
  toWord NoAlias = 4
  toWord NoCapture = 5
  toWord NoWrite = 6
  toWord NoReadWrite = 7

  fromWord 0 = Just Zext
  fromWord 1 = Just Sext
  fromWord 2 = Just ByVal
  fromWord 3 = Just Sret
  fromWord 4 = Just NoAlias
  fromWord 5 = Just NoCapture
  fromWord 6 = Just NoWrite
  fromWord 7 = Just NoReadWrite
  fromWord _ = Nothing

  requiredCapabilities Zext = [Capability.Kernel]
  requiredCapabilities Sext = [Capability.Kernel]
  requiredCapabilities ByVal = [Capability.Kernel]
  requiredCapabilities Sret = [Capability.Kernel]
  requiredCapabilities NoAlias = [Capability.Kernel]
  requiredCapabilities NoCapture = [Capability.Kernel]
  requiredCapabilities NoWrite = [Capability.Kernel]
  requiredCapabilities NoReadWrite = [Capability.Kernel]
  
