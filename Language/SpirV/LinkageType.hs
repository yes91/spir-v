{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.LinkageType where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Associate a linkage type to functions or global variables. See <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Linkage linkage>.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_linkage_type_a_linkage_type Source for Linkage Type>
data LinkageType =
    -- | Accessible by other modules as well.
    Export
    -- | A declaration of a global variable or a function that exists in another module.
  | Import
  deriving(Read, Show, Eq, Ord)

instance SpirEnum LinkageType Word32 where
  toWord Export = 0
  toWord Import = 1

  fromWord 0 = Just Export
  fromWord 1 = Just Import
  fromWord _ = Nothing

  requiredCapabilities Export = [Capability.Linkage]
  requiredCapabilities Import = [Capability.Linkage]
  
