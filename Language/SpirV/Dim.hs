{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.Dim where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Dimensionality of an image. Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_dim_a_dim Source for Dim>
data Dim =
  
    OneD
  
  | TwoD
  
  | ThreeD
  
  | Cube
  
  | Rect
  
  | Buffer
  
  | SubpassData
  deriving(Read, Show, Eq, Ord)

instance SpirEnum Dim Word32 where
  toWord OneD = 0
  toWord TwoD = 1
  toWord ThreeD = 2
  toWord Cube = 3
  toWord Rect = 4
  toWord Buffer = 5
  toWord SubpassData = 6

  fromWord 0 = Just OneD
  fromWord 1 = Just TwoD
  fromWord 2 = Just ThreeD
  fromWord 3 = Just Cube
  fromWord 4 = Just Rect
  fromWord 5 = Just Buffer
  fromWord 6 = Just SubpassData
  fromWord _ = Nothing

  requiredCapabilities OneD = [Capability.Sampled1D]
  requiredCapabilities Cube = [Capability.Shader]
  requiredCapabilities Rect = [Capability.SampledRect]
  requiredCapabilities Buffer = [Capability.SampledBuffer]
  requiredCapabilities SubpassData = [Capability.InputAttachment]
  requiredCapabilities _ = []
