{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.FPRoundingMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Associate a rounding mode to a floating-point conversion instruction.
--
-- By default
--
-- -   Conversions from floating-point to integer types use the round-toward-zero rounding mode.
--
-- -   Conversions to floating-point types use the round-to-nearest-even rounding mode.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_fp_rounding_mode_a_fp_rounding_mode Source for FP Rounding Mode>
data FPRoundingMode =
    -- | Round to nearest even.
    RTE
    -- | Round towards zero.
  | RTZ
    -- | Round towards positive infinity.
  | RTP
    -- | Round towards negative infinity.
  | RTN
  deriving(Read, Show, Eq, Ord)

instance SpirEnum FPRoundingMode Word32 where
  toWord RTE = 0
  toWord RTZ = 1
  toWord RTP = 2
  toWord RTN = 3

  fromWord 0 = Just RTE
  fromWord 1 = Just RTZ
  fromWord 2 = Just RTP
  fromWord 3 = Just RTN
  fromWord _ = Nothing

  requiredCapabilities RTE = [Capability.Kernel]
  requiredCapabilities RTZ = [Capability.Kernel]
  requiredCapabilities RTP = [Capability.Kernel]
  requiredCapabilities RTN = [Capability.Kernel]
  
