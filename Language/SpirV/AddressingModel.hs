{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.AddressingModel where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemoryModel OpMemoryModel>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_addressing_model_a_addressing_model Source for Addressing Model>
data AddressingModel =
  
    Logical
    -- | Indicates a 32-bit module, where the address width is equal to 32 bits.
  | Physical32
    -- | Indicates a 64-bit module, where the address width is equal to 64 bits.
  | Physical64
  deriving(Read, Show, Eq, Ord)

instance SpirEnum AddressingModel Word32 where
  toWord Logical = 0
  toWord Physical32 = 1
  toWord Physical64 = 2

  fromWord 0 = Just Logical
  fromWord 1 = Just Physical32
  fromWord 2 = Just Physical64
  fromWord _ = Nothing

  requiredCapabilities Physical32 = [Capability.Addresses]
  requiredCapabilities Physical64 = [Capability.Addresses]
  requiredCapabilities _ = []
