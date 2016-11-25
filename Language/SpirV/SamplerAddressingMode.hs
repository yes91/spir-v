{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.SamplerAddressingMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Addressing mode for creating constant samplers. Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantSampler OpConstantSampler>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_sampler_addressing_mode_a_sampler_addressing_mode Source for Sampler Addressing Mode>
data SamplerAddressingMode =
    -- | The image coordinates used to sample elements of the image refer to a location inside the image, otherwise the results are undefined.
    None
    -- | Out-of-range image coordinates are clamped to the extent.
  | ClampToEdge
    -- | Out-of-range image coordinates will return a border color.
  | Clamp
    -- | Out-of-range image coordinates are wrapped to the valid range. Can only be used with normalized coordinates.
  | Repeat
    -- | Flip the image coordinate at every integer junction. Can only be used with normalized coordinates.
  | RepeatMirrored
  deriving(Read, Show, Eq, Ord)

instance SpirEnum SamplerAddressingMode Word32 where
  toWord None = 0
  toWord ClampToEdge = 1
  toWord Clamp = 2
  toWord Repeat = 3
  toWord RepeatMirrored = 4

  fromWord 0 = Just None
  fromWord 1 = Just ClampToEdge
  fromWord 2 = Just Clamp
  fromWord 3 = Just Repeat
  fromWord 4 = Just RepeatMirrored
  fromWord _ = Nothing

  requiredCapabilities None = [Capability.Kernel]
  requiredCapabilities ClampToEdge = [Capability.Kernel]
  requiredCapabilities Clamp = [Capability.Kernel]
  requiredCapabilities Repeat = [Capability.Kernel]
  requiredCapabilities RepeatMirrored = [Capability.Kernel]
  
