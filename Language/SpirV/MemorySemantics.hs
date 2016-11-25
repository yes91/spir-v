{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.MemorySemantics where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Must be an /\<id>/ of a 32-bit integer scalar that contains a mask. The rest of this description is about that mask.
--
-- Memory semantics define memory-order constraints, and on what storage classes those constraints apply to. The memory order constrains the allowed orders in which memory operations in this <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation> can made visible to another invocation. The storage classes specify to which subsets of memory these constraints are to be applied. Storage classes not selected are not being constrained.
--
--  Despite being a mask and allowing multiple bits to be combined, at most one of the first four (low-order) bits can be set. Requesting both __Acquire__ and __Release__ semantics is done by setting the __AcquireRelease__ bit, not by setting two bits.
--
-- This value is a mask; it can be formed by combining the bits from multiple rows in the table below.
--
-- Used by:
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpControlBarrier OpControlBarrier>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemoryBarrier OpMemoryBarrier>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicLoad OpAtomicLoad>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicStore OpAtomicStore>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicExchange OpAtomicExchange>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicCompareExchange OpAtomicCompareExchange>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicCompareExchangeWeak OpAtomicCompareExchangeWeak>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicIIncrement OpAtomicIIncrement>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicIDecrement OpAtomicIDecrement>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicIAdd OpAtomicIAdd>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicISub OpAtomicISub>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicSMin OpAtomicSMin>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicUMin OpAtomicUMin>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicSMax OpAtomicSMax>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicUMax OpAtomicUMax>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicAnd OpAtomicAnd>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicOr OpAtomicOr>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicXor OpAtomicXor>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicFlagTestAndSet OpAtomicFlagTestAndSet>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicFlagClear OpAtomicFlagClear>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemoryNamedBarrier OpMemoryNamedBarrier>__
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_memory_semantics__id_a_memory_semantics_lt_id_gt Source for Memory Semantics>
data MemorySemantics =
  
    NoneRelaxed
    -- | All memory operations provided in program order after this memory operation will execute after this memory operation.
  | Acquire
    -- | All memory operations provided in program order before this memory operation will execute before this memory operation.
  | Release
    -- | Has the properties of both __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Memory_Semantics Acquire>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Memory_Semantics Release>__ semantics. It is used for read-modify-write operations.
  | AcquireRelease
    -- | All observers will see this memory access in the same order with respect to other sequentially-consistent memory accesses from this <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation>.
  | SequentiallyConsistent
    -- | Apply the memory-ordering constraints to __Uniform__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> memory.
  | UniformMemory
    -- | Apply the memory-ordering constraints to subgroup memory.
  | SubgroupMemory
    -- | Apply the memory-ordering constraints to __Workgroup__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> memory.
  | WorkgroupMemory
    -- | Apply the memory-ordering constraints to __CrossWorkgroup__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> memory.
  | CrossWorkgroupMemory
    -- | Apply the memory-ordering constraints to __AtomicCounter__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> memory.
  | AtomicCounterMemory
    -- | Apply the memory-ordering constraints to image contents (types declared by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__), or to accesses done through pointers to the __Image__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class>.
  | ImageMemory
  deriving(Read, Show, Eq, Ord)

instance SpirEnum MemorySemantics Word32 where
  toWord NoneRelaxed = 0
  toWord Acquire = 2
  toWord Release = 4
  toWord AcquireRelease = 8
  toWord SequentiallyConsistent = 16
  toWord UniformMemory = 64
  toWord SubgroupMemory = 128
  toWord WorkgroupMemory = 256
  toWord CrossWorkgroupMemory = 512
  toWord AtomicCounterMemory = 1024
  toWord ImageMemory = 2048

  fromWord 0 = Just NoneRelaxed
  fromWord 2 = Just Acquire
  fromWord 4 = Just Release
  fromWord 8 = Just AcquireRelease
  fromWord 16 = Just SequentiallyConsistent
  fromWord 64 = Just UniformMemory
  fromWord 128 = Just SubgroupMemory
  fromWord 256 = Just WorkgroupMemory
  fromWord 512 = Just CrossWorkgroupMemory
  fromWord 1024 = Just AtomicCounterMemory
  fromWord 2048 = Just ImageMemory
  fromWord _ = Nothing

  requiredCapabilities UniformMemory = [Capability.Shader]
  requiredCapabilities AtomicCounterMemory = [Capability.AtomicStorage]
  requiredCapabilities _ = []
