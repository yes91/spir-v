{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.FPFastMathMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Enables fast math operations which are otherwise unsafe.
--
-- -   Only valid on __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFAdd OpFAdd>__, __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFSub OpFSub>__, __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFMul OpFMul>__, __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFDiv OpFDiv>__, __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFRem OpFRem>__, and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFMod OpFMod>__ instructions.
--
-- This value is a mask; it can be formed by combining the bits from multiple rows in the table below.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_fp_fast_math_mode_a_fp_fast_math_mode Source for FP Fast Math Mode>
data FPFastMathMode =
  
    None
    -- | Assume parameters and result are not NaN.
  | NotNaN
    -- | Assume parameters and result are not +\/- Inf.
  | NotInf
    -- | Treat the sign of a zero parameter or result as insignificant.
  | NSZ
    -- | Allow the usage of reciprocal rather than perform a division.
  | AllowRecip
    -- | Allow algebraic transformations according to real-number associative and distributive algebra. This flag implies all the others.
  | Fast
  deriving(Read, Show, Eq, Ord)

instance SpirEnum FPFastMathMode Word32 where
  toWord None = 0
  toWord NotNaN = 1
  toWord NotInf = 2
  toWord NSZ = 4
  toWord AllowRecip = 8
  toWord Fast = 16

  fromWord 0 = Just None
  fromWord 1 = Just NotNaN
  fromWord 2 = Just NotInf
  fromWord 4 = Just NSZ
  fromWord 8 = Just AllowRecip
  fromWord 16 = Just Fast
  fromWord _ = Nothing

  requiredCapabilities NotNaN = [Capability.Kernel]
  requiredCapabilities NotInf = [Capability.Kernel]
  requiredCapabilities NSZ = [Capability.Kernel]
  requiredCapabilities AllowRecip = [Capability.Kernel]
  requiredCapabilities Fast = [Capability.Kernel]
  requiredCapabilities _ = []
