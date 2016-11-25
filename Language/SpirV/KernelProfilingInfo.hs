{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.KernelProfilingInfo where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Specify the profiling information to be queried. Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCaptureEventProfilingInfo OpCaptureEventProfilingInfo>__.
--
-- This value is a mask; it can be formed by combining the bits from multiple rows in the table below.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_kernel_profiling_info_a_kernel_profiling_info Source for Kernel Profiling Info>
data KernelProfilingInfo =
  
    None
    -- | Indicates that the profiling info queried is the execution time.
  | CmdExecTime
  deriving(Read, Show, Eq, Ord)

instance SpirEnum KernelProfilingInfo Word32 where
  toWord None = 0
  toWord CmdExecTime = 1

  fromWord 0 = Just None
  fromWord 1 = Just CmdExecTime
  fromWord _ = Nothing

  requiredCapabilities CmdExecTime = [Capability.Kernel]
  requiredCapabilities _ = []
