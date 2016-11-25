{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.KernelEnqueueFlags where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Specify when the child kernel begins execution.
--
--  __Note:__ Implementations are not required to honor this flag. Implementations may not schedule kernel launch earlier than the point specified by this flag, however. Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEnqueueKernel OpEnqueueKernel>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_kernel_enqueue_flags_a_kernel_enqueue_flags Source for Kernel Enqueue Flags>
data KernelEnqueueFlags =
    -- | Indicates that the enqueued kernels do not need to wait for the parent kernel to finish execution before they begin execution.
    NoWait
    -- | Indicates that all work-items of the parent kernel must finish executing and all immediate side effects committed before the enqueued child kernel may begin execution.
    --
    --  __Note:__ Immediate meaning not side effects resulting from child kernels. The side effects would include stores to global memory and pipe reads and writes.
  | WaitKernel
    -- | Indicates that the enqueued kernels wait only for the workgroup that enqueued the kernels to finish before they begin execution.
    --
    --  __Note:__ This acts as a memory synchronization point between work-items in a work-group and child kernels enqueued by work-items in the work-group.
  | WaitWorkGroup
  deriving(Read, Show, Eq, Ord)

instance SpirEnum KernelEnqueueFlags Word32 where
  toWord NoWait = 0
  toWord WaitKernel = 1
  toWord WaitWorkGroup = 2

  fromWord 0 = Just NoWait
  fromWord 1 = Just WaitKernel
  fromWord 2 = Just WaitWorkGroup
  fromWord _ = Nothing

  requiredCapabilities NoWait = [Capability.Kernel]
  requiredCapabilities WaitKernel = [Capability.Kernel]
  requiredCapabilities WaitWorkGroup = [Capability.Kernel]
  
