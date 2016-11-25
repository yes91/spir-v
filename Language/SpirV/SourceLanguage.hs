{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.SourceLanguage where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | The source language is for debug purposes only, with no semantics that affect the meaning of other parts of the module. Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSource OpSource>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_source_language_a_source_language Source for Source Language>
data SourceLanguage =
  
    Unknown
  
  | ESSL
  
  | GLSL
  
  | OpenCLC
  
  | OpenCLCPP
  deriving(Read, Show, Eq, Ord)

instance SpirEnum SourceLanguage Word32 where
  toWord Unknown = 0
  toWord ESSL = 1
  toWord GLSL = 2
  toWord OpenCLC = 3
  toWord OpenCLCPP = 4

  fromWord 0 = Just Unknown
  fromWord 1 = Just ESSL
  fromWord 2 = Just GLSL
  fromWord 3 = Just OpenCLC
  fromWord 4 = Just OpenCLCPP
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
