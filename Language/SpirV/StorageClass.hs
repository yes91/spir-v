{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.StorageClass where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Class of storage for declared variables (does not include /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Intermediate intermediate>/ values). Used by:
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypePointer OpTypePointer>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeForwardPointer OpTypeForwardPointer>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVariable OpVariable>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGenericCastToPtrExplicit OpGenericCastToPtrExplicit>__
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_storage_class_a_storage_class Source for Storage Class>
data StorageClass =
    -- | Shared externally, visible across all functions in all <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in all work groups. Graphics uniform memory. OpenCL constant memory. Variables declared with this storage class are read-only, and cannot have initializers.
    UniformConstant
    -- | Input from pipeline. Visible across all functions in the current <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation>. Variables declared with this storage class are read-only, and cannot have initializers.
  | Input
    -- | Shared externally, visible across all functions in all <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in all work groups. Graphics uniform blocks and buffer blocks.
  | Uniform
    -- | Output to pipeline. Visible across all functions in the current <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation>.
  | Output
    -- | Shared across all <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> within a work group. Visible across all functions. The OpenGL \"shared\" storage qualifier. OpenCL local memory.
  | Workgroup
    -- | Visible across all functions of all <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> of all work groups. OpenCL global memory.
  | CrossWorkgroup
    -- | Visible to all functions in the current <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation>. Regular global memory.
  | Private
    -- | Visible only within the declaring function of the current <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation>. Regular function memory.
  | Function
    -- | For generic pointers, which overload the __Function__, __Workgroup__, and __CrossWorkgroup__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>.
  | Generic
    -- | For holding push-constant memory, visible across all functions in all <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in all work groups. Intended to contain a small bank of values pushed from the API. Variables declared with this storage class are read-only, and cannot have initializers.
  | PushConstant
    -- | For holding atomic counters. Visible across all functions of the current <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation>. Atomic counter-specific memory.
  | AtomicCounter
    -- | For holding <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#ImageTerm image> memory.
  | Image
  deriving(Read, Show, Eq, Ord)

instance SpirEnum StorageClass Word32 where
  toWord UniformConstant = 0
  toWord Input = 1
  toWord Uniform = 2
  toWord Output = 3
  toWord Workgroup = 4
  toWord CrossWorkgroup = 5
  toWord Private = 6
  toWord Function = 7
  toWord Generic = 8
  toWord PushConstant = 9
  toWord AtomicCounter = 10
  toWord Image = 11

  fromWord 0 = Just UniformConstant
  fromWord 1 = Just Input
  fromWord 2 = Just Uniform
  fromWord 3 = Just Output
  fromWord 4 = Just Workgroup
  fromWord 5 = Just CrossWorkgroup
  fromWord 6 = Just Private
  fromWord 7 = Just Function
  fromWord 8 = Just Generic
  fromWord 9 = Just PushConstant
  fromWord 10 = Just AtomicCounter
  fromWord 11 = Just Image
  fromWord _ = Nothing

  requiredCapabilities Uniform = [Capability.Shader]
  requiredCapabilities Output = [Capability.Shader]
  requiredCapabilities Private = [Capability.Shader]
  requiredCapabilities Generic = [Capability.GenericPointer]
  requiredCapabilities PushConstant = [Capability.Shader]
  requiredCapabilities AtomicCounter = [Capability.AtomicStorage]
  requiredCapabilities _ = []
