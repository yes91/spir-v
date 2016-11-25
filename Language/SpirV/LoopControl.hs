{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.LoopControl where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | This value is a mask; it can be formed by combining the bits from multiple rows in the table below.
--
-- Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLoopMerge OpLoopMerge>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_loop_control_a_loop_control Source for Loop Control>
data LoopControl =
  
    None
    -- | Strong request, to the extent possible, to unroll or unwind this loop.
  | Unroll
    -- | Strong request, to the extent possible, to keep this loop as a loop, without unrolling.
  | DontUnroll
    -- | Guarantees that there are no dependencies between loop iterations.
  | DependencyInfinite
    -- | Guarantees that there are no dependencies between a number of loop iterations,specified as a subsequent literal-number operand to the instruction.
  | DependencyLength
  deriving(Read, Show, Eq, Ord)

instance SpirEnum LoopControl Word32 where
  toWord None = 0
  toWord Unroll = 1
  toWord DontUnroll = 2
  toWord DependencyInfinite = 4
  toWord DependencyLength = 8

  fromWord 0 = Just None
  fromWord 1 = Just Unroll
  fromWord 2 = Just DontUnroll
  fromWord 4 = Just DependencyInfinite
  fromWord 8 = Just DependencyLength
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
