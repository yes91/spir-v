{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ImageOperands where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Additional operands to sampling, or getting texels from, an image. Bits that are set can indicate that another operand follows. If there are multiple following operands indicated, they are ordered: Those indicated by smaller-numbered bits appear first. At least one bit must be set (__None__ is invalid).
--
-- This value is a mask; it can be formed by combining the bits from multiple rows in the table below.
--
-- Used by:
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleImplicitLod OpImageSampleImplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleExplicitLod OpImageSampleExplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleDrefImplicitLod OpImageSampleDrefImplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleDrefExplicitLod OpImageSampleDrefExplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleProjImplicitLod OpImageSampleProjImplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleProjExplicitLod OpImageSampleProjExplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleProjDrefImplicitLod OpImageSampleProjDrefImplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleProjDrefExplicitLod OpImageSampleProjDrefExplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageFetch OpImageFetch>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageGather OpImageGather>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageDrefGather OpImageDrefGather>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageRead OpImageRead>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageWrite OpImageWrite>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleImplicitLod OpImageSparseSampleImplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleExplicitLod OpImageSparseSampleExplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleDrefImplicitLod OpImageSparseSampleDrefImplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleDrefExplicitLod OpImageSparseSampleDrefExplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleProjImplicitLod OpImageSparseSampleProjImplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleProjExplicitLod OpImageSparseSampleProjExplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleProjDrefImplicitLod OpImageSparseSampleProjDrefImplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleProjDrefExplicitLod OpImageSparseSampleProjDrefExplicitLod>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseFetch OpImageSparseFetch>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseGather OpImageSparseGather>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseDrefGather OpImageSparseDrefGather>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseRead OpImageSparseRead>__
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_image_operands_a_image_operands Source for Image Operands>
data ImageOperands =
  
    None
    -- | A following operand is the bias added to the implicit level of detail. Only valid with implicit-lod instructions. It must be a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Floating floating-point type>/ scalar. This can only be used with an __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ that has a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim>/ operand of __1D__, __2D__, __3D__, or __Cube__, and the /MS/ operand must be 0.
  | Bias
    -- | A following operand is the explicit level-of-detail to use. Only valid with explicit-lod instructions. For sampling operations, it must be a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Floating floating-point type>/ scalar. For queries and fetch operations, it must be an /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Integer integer type>/ scalar. This can only be used with an __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ that has a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim>/ operand of __1D__, __2D__, __3D__, or __Cube__, and the /MS/ operand must be 0.
  | Lod
    -- | Two following operands are /dx/ followed by /dy/. These are explicit derivatives in the /x/ and /y/ direction to use in computing level of detail. Each is a scalar or vector containing (/du\/dx/[, /dv\/dx/] [, /dw\/dx/]) and (/du\/dy/[, /dv\/dy/] [, /dw\/dy/]). The number of components of each must equal the number of components in /Coordinate/, minus the /array layer/ component, if present. Only valid with explicit-lod instructions. They must be a scalar or vector of /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Floating floating-point type>/. This can only be used with an __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ that has an /MS/ operand of 0. It is invalid to set both the __Lod__ and __Grad__ bits.
  | Grad
    -- | A following operand is added to (/u/, /v/, /w/) before texel lookup. It must be an /\<id>/ of an integer-based /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#ConstantInstruction constant instruction>/ of scalar or vector type. It is a compile-time error if these fall outside a target-dependent allowed range. The number of components must equal the number of components in /Coordinate/, minus the /array layer/ component, if present.
  | ConstOffset
    -- | A following operand is added to (/u/, /v/, /w/) before texel lookup. It must be a scalar or vector of /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Integer integer type>/. It is a compile-time error if these fall outside a target-dependent allowed range. The number of components must equal the number of components in /Coordinate/, minus the /array layer/ component, if present.
  | Offset
    -- | A following operand is /Offsets/. /Offsets/ must be an /\<id>/ of a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#ConstantInstruction constant instruction>/ making an array of size four of vectors of two integer components. Each gathered texel is identified by adding one of these array elements to the (/u/, /v/) sampled location. It is a compile-time error if this falls outside a target-dependent allowed range. Only valid with __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageGather OpImageGather>__ or __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageDrefGather OpImageDrefGather>__.
  | ConstOffsets
    -- | A following operand is the sample number of the sample to use. Only valid with __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageFetch OpImageFetch>__, __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageRead OpImageRead>__, and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageWrite OpImageWrite>__. It is invalid to have a __Sample__ operand if the underlying __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ has /MS/ of 0. It must be an /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Integer integer type>/ scalar.
  | Sample
    -- | A following operand is the minimum level-of-detail to use when accessing the image. Only valid with __Implicit__ instructions and __Grad__ instructions. It must be a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Floating floating-point type>/ scalar. This can only be used with an __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ that has a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim>/ operand of __1D__, __2D__, __3D__, or __Cube__, and the /MS/ operand must be 0.
  | MinLod
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ImageOperands Word32 where
  toWord None = 0
  toWord Bias = 1
  toWord Lod = 2
  toWord Grad = 4
  toWord ConstOffset = 8
  toWord Offset = 16
  toWord ConstOffsets = 32
  toWord Sample = 64
  toWord MinLod = 128

  fromWord 0 = Just None
  fromWord 1 = Just Bias
  fromWord 2 = Just Lod
  fromWord 4 = Just Grad
  fromWord 8 = Just ConstOffset
  fromWord 16 = Just Offset
  fromWord 32 = Just ConstOffsets
  fromWord 64 = Just Sample
  fromWord 128 = Just MinLod
  fromWord _ = Nothing

  requiredCapabilities Bias = [Capability.Shader]
  requiredCapabilities Offset = [Capability.ImageGatherExtended]
  requiredCapabilities MinLod = [Capability.MinLod]
  requiredCapabilities _ = []
