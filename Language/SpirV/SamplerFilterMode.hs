{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.SamplerFilterMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Filter mode for creating constant samplers. Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantSampler OpConstantSampler>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_sampler_filter_mode_a_sampler_filter_mode Source for Sampler Filter Mode>
data SamplerFilterMode =
    -- | Use filter nearest mode when performing a read image operation.
    Nearest
    -- | Use filter linear mode when performing a read image operation.
  | Linear
  deriving(Read, Show, Eq, Ord)

instance SpirEnum SamplerFilterMode Word32 where
  toWord Nearest = 0
  toWord Linear = 1

  fromWord 0 = Just Nearest
  fromWord 1 = Just Linear
  fromWord _ = Nothing

  requiredCapabilities Nearest = [Capability.Kernel]
  requiredCapabilities Linear = [Capability.Kernel]
  
