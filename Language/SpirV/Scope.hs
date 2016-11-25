{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.Scope where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Must be an /\<id>/ of a 32-bit integer scalar that contains a mask. The rest of this description is about that mask.
--
-- The execution scope or memory scope of an operation. When used as a memory scope, it specifies the distance of synchronization from the current <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation>. When used as an execution scope, it specifies the set of executing invocations taking part in the operation. Used by:
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
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupAsyncCopy OpGroupAsyncCopy>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupWaitEvents OpGroupWaitEvents>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupAll OpGroupAll>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupAny OpGroupAny>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupBroadcast OpGroupBroadcast>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupIAdd OpGroupIAdd>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFAdd OpGroupFAdd>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFMin OpGroupFMin>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupUMin OpGroupUMin>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupSMin OpGroupSMin>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFMax OpGroupFMax>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupUMax OpGroupUMax>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupSMax OpGroupSMax>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupReserveReadPipePackets OpGroupReserveReadPipePackets>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupReserveWritePipePackets OpGroupReserveWritePipePackets>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupCommitReadPipe OpGroupCommitReadPipe>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupCommitWritePipe OpGroupCommitWritePipe>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicFlagTestAndSet OpAtomicFlagTestAndSet>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicFlagClear OpAtomicFlagClear>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemoryNamedBarrier OpMemoryNamedBarrier>__
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_scope__id_a_scope_lt_id_gt Source for Scope>
data Scope =
    -- | Scope crosses multiple devices.
    CrossDevice
    -- | Scope is the current device.
  | Device
    -- | Scope is the current workgroup.
  | Workgroup
    -- | Scope is the current subgroup.
  | Subgroup
    -- | Scope is the current <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation Invocation>.
  | Invocation
  deriving(Read, Show, Eq, Ord)

instance SpirEnum Scope Word32 where
  toWord CrossDevice = 0
  toWord Device = 1
  toWord Workgroup = 2
  toWord Subgroup = 3
  toWord Invocation = 4

  fromWord 0 = Just CrossDevice
  fromWord 1 = Just Device
  fromWord 2 = Just Workgroup
  fromWord 3 = Just Subgroup
  fromWord 4 = Just Invocation
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
