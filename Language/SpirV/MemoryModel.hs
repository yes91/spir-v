{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.MemoryModel where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemoryModel OpMemoryModel>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_memory_model_a_memory_model Source for Memory Model>
data MemoryModel =
    -- | No shared memory consistency issues.
    Simple
    -- | Memory model needed by later versions of GLSL and ESSL. Works across multiple versions.
  | GLSL450
    -- | OpenCL memory model.
  | OpenCL
  deriving(Read, Show, Eq, Ord)

instance SpirEnum MemoryModel Word32 where
  toWord Simple = 0
  toWord GLSL450 = 1
  toWord OpenCL = 2

  fromWord 0 = Just Simple
  fromWord 1 = Just GLSL450
  fromWord 2 = Just OpenCL
  fromWord _ = Nothing

  requiredCapabilities Simple = [Capability.Shader]
  requiredCapabilities GLSL450 = [Capability.Shader]
  requiredCapabilities OpenCL = [Capability.Kernel]
  
