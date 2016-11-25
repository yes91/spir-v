{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.AccessQualifier where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Defines the access permissions.
--
-- Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypePipe OpTypePipe>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_access_qualifier_a_access_qualifier Source for Access Qualifier>
data AccessQualifier =
    -- | A read-only object.
    ReadOnly
    -- | A write-only object.
  | WriteOnly
    -- | A readable and writable object.
  | ReadWrite
  deriving(Read, Show, Eq, Ord)

instance SpirEnum AccessQualifier Word32 where
  toWord ReadOnly = 0
  toWord WriteOnly = 1
  toWord ReadWrite = 2

  fromWord 0 = Just ReadOnly
  fromWord 1 = Just WriteOnly
  fromWord 2 = Just ReadWrite
  fromWord _ = Nothing

  requiredCapabilities ReadOnly = [Capability.Kernel]
  requiredCapabilities WriteOnly = [Capability.Kernel]
  requiredCapabilities ReadWrite = [Capability.Kernel]
  
