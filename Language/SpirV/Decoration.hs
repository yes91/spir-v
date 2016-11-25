{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.Decoration where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDecorate OpDecorate>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemberDecorate OpMemberDecorate>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_decoration_a_decoration Source for Decoration>
data Decoration =
    -- | Allow reduced precision operations. To be used as described in <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#RelaxedPrecisionSection Relaxed Precision>.
    RelaxedPrecision
    -- | Apply to a scalar specialization constant. Forms the API linkage for setting a specialized value. See <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#SpecializationSection specialization>.
  | SpecId
    -- | Apply to a structure type to establish it is a non-SSBO-like shader-interface block.
  | Block
    -- | Apply to a structure type to establish it is an SSBO-like shader-interface block.
  | BufferBlock
    -- | Applies only to a member of a structure type. Only valid on a matrix or array whose most basic element is a matrix. Indicates that components within a row are contiguous in memory.
  | RowMajor
    -- | Applies only to a member of a structure type. Only valid on a matrix or array whose most basic element is a matrix. Indicates that components within a column are contiguous in memory.
  | ColMajor
    -- | Apply to an array type to specify the stride, in bytes, of the array’s elements. Must not be applied to anything other than an array type.
  | ArrayStride
    -- | Applies only to a member of a structure type. Only valid on a matrix or array whose most basic element is a matrix. Specifies the stride of rows in a __RowMajor__-decorated matrix, or columns in a __ColMajor__-decorated matrix.
  | MatrixStride
    -- | Apply to a structure type to get GLSL __shared__ memory layout.
  | GLSLShared
    -- | Apply to a structure type to get GLSL __packed__ memory layout.
  | GLSLPacked
    -- | Apply to a structure type, to marks it as \"packed\", indicating that the alignment of the structure is one and that there is no padding between structure members.
  | CPacked
    -- | Apply to an object or a member of a structure type. Indicates which built-in variable the entity represents. See <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#BuiltIn BuiltIn> for more information.
  | BuiltIn
    -- | Apply to an object or a member of a structure type. Indicates that linear, non-perspective correct, interpolation must be used. Only valid for the __Input__ and __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>.
  | NoPerspective
    -- | Apply to an object or a member of a structure type. Indicates no interpolation will be done. The non-interpolated value will come from a vertex, as described in the API specification. Only valid for the __Input__ and __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>.
  | Flat
    -- | Apply to an object or a member of a structure type. Indicates a tessellation patch. Only valid for the __Input__ and __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>. Invalid to use on objects or types referenced by non-tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | Patch
    -- | Apply to an object or a member of a structure type. When used with multi-sampling rasterization, allows a single interpolation location for an entire pixel. The interpolation location must lie in both the pixel and in the primitive being rasterized. Only valid for the __Input__ and __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>.
  | Centroid
    -- | Apply to an object or a member of a structure type. When used with multi-sampling rasterization, requires per-sample interpolation. The interpolation locations must be the locations of the samples lying in both the pixel and in the primitive being rasterized. Only valid for the __Input__ and __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>.
  | Sample
    -- | Apply to a variable, to indicate expressions computing its value be done invariant with respect to other modules computing the same expressions.
  | Invariant
    -- | Apply to a variable, to indicate the compiler may compile as if there is no aliasing. See the <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#AliasingSection Aliasing> section for more detail.
  | Restrict
    -- | Apply to a variable, to indicate the compiler is to generate accesses to the variable that work correctly in the presence of aliasing. See the <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#AliasingSection Aliasing> section for more detail.
  | Aliased
    -- | Apply to an object or a member of a structure type. Can only be used for objects declared as storage images (see __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__) or in the __Uniform__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> with the __BufferBlock__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decoration>. This indicates the memory holding the variable is volatile memory. Accesses to volatile memory cannot be eliminated, duplicated, or combined with other accesses. The variable cannot be in the __Function__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class>.
  | Volatile
    -- | Indicates that a global variable is constant and will __never__ be modified. Only allowed on global variables.
  | Constant
    -- | Apply to an object or a member of a structure type. Can only be used for objects declared as storage images (see __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__) or in the __Uniform__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> with the __BufferBlock__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decoration>. This indicates the memory backing the object is coherent.
  | Coherent
    -- | Apply to an object or a member of a structure type. Can only be used for objects declared as storage images (see __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__) or in the __Uniform__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> with the __BufferBlock__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decoration>. This indicates the memory holding the variable is not writable, and that this module does not write to it.
  | NonWritable
    -- | Apply to an object or a member of a structure type. Can only be used for objects declared as storage images (see __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__) or in the __Uniform__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> with the __BufferBlock__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decoration>. This indicates the memory holding the variable is not readable, and that this module does not read from it.
  | NonReadable
    -- | Apply to an object or a member of a structure type. Asserts that the value backing the decorated /\<id>/ is <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#DynamicallyUniform dynamically uniform>, hence the consumer is allowed to assume this is the case.
  | Uniform
    -- | Indicates that a conversion to an integer type which is outside the representable range of /Result Type/ will be clamped to the nearest representable value of /Result Type/. /NaN/ will be converted to /0/.
    --
    --  This decoration can only be applied to conversion instructions to integer types, not including the __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSatConvertUToS OpSatConvertUToS>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSatConvertSToU OpSatConvertSToU>__ instructions.
  | SaturatedConversion
    -- | Apply to an object or a member of a structure type. Indicates the stream number to put an output on. Only valid for the __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> and the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | Stream
    -- | Apply to a variable or a structure-type member. Forms the main linkage for <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> __Input__ and __Output__ variables:
    --
    --  - between the API and vertex-stage inputs,
    --
    --  - between consecutive programmable stages, or
    --
    --  - between fragment-stage outputs and the API.
    --
    --  Also can tag variables or structure-type members in the __UniformConstant__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> for linkage with the API.
    --
    --  Only valid for the __Input__, __Output__, and __UniformConstant__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>.
  | Location
    -- | Apply to an object or a member of a structure type. Indicates which component within a __Location__ will be taken by the decorated entity. Only valid for the __Input__ and __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>.
  | Component
    -- | Apply to a variable to identify a blend equation input index, used as described in the API specification. Only valid for the __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class> and the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | Index
    -- | Apply to a variable. Part of the main linkage between the API and SPIR-V modules for memory buffers, images, etc. See the API specification for more information.
  | Binding
    -- | Apply to a variable. Part of the main linkage between the API and SPIR-V modules for memory buffers, images, etc. See the API specification for more information.
  | DescriptorSet
    -- | Apply to a structure-type member. This gives the byte offset of the member relative to the beginning of the structure. Can be used, for example, by both uniform and transform-feedback buffers. It must not cause any overlap of the structure’s members, or overflow of a transform-feedback buffer’s __XfbStride__.
  | Offset
    -- | Apply to an object or a member of a structure type. Indicates which transform-feedback buffer an output is written to. Only valid for the __Output__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes> of /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#VertexProcessor vertex processing>/ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | XfbBuffer
    -- | Apply to anything __XfbBuffer__ is applied to. Specifies the stride, in bytes, of transform-feedback buffer vertices. If the transform-feedback buffer is capturing any double-precision components, the stride must be a multiple of 8, otherwise it must be a multiple of 4.
  | XfbStride
    -- | Indicates a function return value or parameter attribute.
  | FuncParamAttr
    -- | Indicates a floating-point rounding mode.
  | FPRoundingMode
    -- | Indicates a floating-point fast math flag.
  | FPFastMathMode
    -- | Associate linkage attributes to values. Only valid on __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunction OpFunction>__ or global (module scope) __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVariable OpVariable>__. See <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Linkage linkage>.
  | LinkageAttributes
    -- | Apply to an <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Arithmetic arithmetic instruction> to indicate the operation cannot be combined with another instruction to form a single operation. For example, if applied to an __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFMul OpFMul>__, that multiply can’t be combined with an addition to yield a fused multiply-add operation. Furthermore, such operations are not allowed to reassociate; e.g., add(a + add(b+c)) cannot be transformed to add(add(a+b) + c).
  | NoContraction
    -- | Apply to a variable to provide an input-target index (as described in the API specification). Only valid in the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model> and for variables of type __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ with a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim>/ operand of __SubpassData__.
  | InputAttachmentIndex
    -- | Apply to a pointer. This declares a known minimum alignment the pointer has.
  | Alignment
    -- | Apply to a pointer. This declares a known maximum byte offset this pointer will be incremented by from the point of the decoration. This is a guaranteed upper bound when applied to __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunctionParameter OpFunctionParameter>__.
  | MaxByteOffset
  deriving(Read, Show, Eq, Ord)

instance SpirEnum Decoration Word32 where
  toWord RelaxedPrecision = 0
  toWord SpecId = 1
  toWord Block = 2
  toWord BufferBlock = 3
  toWord RowMajor = 4
  toWord ColMajor = 5
  toWord ArrayStride = 6
  toWord MatrixStride = 7
  toWord GLSLShared = 8
  toWord GLSLPacked = 9
  toWord CPacked = 10
  toWord BuiltIn = 11
  toWord NoPerspective = 13
  toWord Flat = 14
  toWord Patch = 15
  toWord Centroid = 16
  toWord Sample = 17
  toWord Invariant = 18
  toWord Restrict = 19
  toWord Aliased = 20
  toWord Volatile = 21
  toWord Constant = 22
  toWord Coherent = 23
  toWord NonWritable = 24
  toWord NonReadable = 25
  toWord Uniform = 26
  toWord SaturatedConversion = 28
  toWord Stream = 29
  toWord Location = 30
  toWord Component = 31
  toWord Index = 32
  toWord Binding = 33
  toWord DescriptorSet = 34
  toWord Offset = 35
  toWord XfbBuffer = 36
  toWord XfbStride = 37
  toWord FuncParamAttr = 38
  toWord FPRoundingMode = 39
  toWord FPFastMathMode = 40
  toWord LinkageAttributes = 41
  toWord NoContraction = 42
  toWord InputAttachmentIndex = 43
  toWord Alignment = 44
  toWord MaxByteOffset = 45

  fromWord 0 = Just RelaxedPrecision
  fromWord 1 = Just SpecId
  fromWord 2 = Just Block
  fromWord 3 = Just BufferBlock
  fromWord 4 = Just RowMajor
  fromWord 5 = Just ColMajor
  fromWord 6 = Just ArrayStride
  fromWord 7 = Just MatrixStride
  fromWord 8 = Just GLSLShared
  fromWord 9 = Just GLSLPacked
  fromWord 10 = Just CPacked
  fromWord 11 = Just BuiltIn
  fromWord 13 = Just NoPerspective
  fromWord 14 = Just Flat
  fromWord 15 = Just Patch
  fromWord 16 = Just Centroid
  fromWord 17 = Just Sample
  fromWord 18 = Just Invariant
  fromWord 19 = Just Restrict
  fromWord 20 = Just Aliased
  fromWord 21 = Just Volatile
  fromWord 22 = Just Constant
  fromWord 23 = Just Coherent
  fromWord 24 = Just NonWritable
  fromWord 25 = Just NonReadable
  fromWord 26 = Just Uniform
  fromWord 28 = Just SaturatedConversion
  fromWord 29 = Just Stream
  fromWord 30 = Just Location
  fromWord 31 = Just Component
  fromWord 32 = Just Index
  fromWord 33 = Just Binding
  fromWord 34 = Just DescriptorSet
  fromWord 35 = Just Offset
  fromWord 36 = Just XfbBuffer
  fromWord 37 = Just XfbStride
  fromWord 38 = Just FuncParamAttr
  fromWord 39 = Just FPRoundingMode
  fromWord 40 = Just FPFastMathMode
  fromWord 41 = Just LinkageAttributes
  fromWord 42 = Just NoContraction
  fromWord 43 = Just InputAttachmentIndex
  fromWord 44 = Just Alignment
  fromWord 45 = Just MaxByteOffset
  fromWord _ = Nothing

  requiredCapabilities RelaxedPrecision = [Capability.Shader]
  requiredCapabilities SpecId = [Capability.Shader, Capability.Kernel]
  requiredCapabilities Block = [Capability.Shader]
  requiredCapabilities BufferBlock = [Capability.Shader]
  requiredCapabilities RowMajor = [Capability.Matrix]
  requiredCapabilities ColMajor = [Capability.Matrix]
  requiredCapabilities ArrayStride = [Capability.Shader]
  requiredCapabilities MatrixStride = [Capability.Matrix]
  requiredCapabilities GLSLShared = [Capability.Shader]
  requiredCapabilities GLSLPacked = [Capability.Shader]
  requiredCapabilities CPacked = [Capability.Kernel]
  requiredCapabilities NoPerspective = [Capability.Shader]
  requiredCapabilities Flat = [Capability.Shader]
  requiredCapabilities Patch = [Capability.Tessellation]
  requiredCapabilities Centroid = [Capability.Shader]
  requiredCapabilities Sample = [Capability.SampleRateShading]
  requiredCapabilities Invariant = [Capability.Shader]
  requiredCapabilities Constant = [Capability.Kernel]
  requiredCapabilities Uniform = [Capability.Shader]
  requiredCapabilities SaturatedConversion = [Capability.Kernel]
  requiredCapabilities Stream = [Capability.GeometryStreams]
  requiredCapabilities Location = [Capability.Shader]
  requiredCapabilities Component = [Capability.Shader]
  requiredCapabilities Index = [Capability.Shader]
  requiredCapabilities Binding = [Capability.Shader]
  requiredCapabilities DescriptorSet = [Capability.Shader]
  requiredCapabilities Offset = [Capability.Shader]
  requiredCapabilities XfbBuffer = [Capability.TransformFeedback]
  requiredCapabilities XfbStride = [Capability.TransformFeedback]
  requiredCapabilities FuncParamAttr = [Capability.Kernel]
  requiredCapabilities FPRoundingMode = [Capability.Kernel]
  requiredCapabilities FPFastMathMode = [Capability.Kernel]
  requiredCapabilities LinkageAttributes = [Capability.Linkage]
  requiredCapabilities NoContraction = [Capability.Shader]
  requiredCapabilities InputAttachmentIndex = [Capability.InputAttachment]
  requiredCapabilities Alignment = [Capability.Kernel]
  requiredCapabilities MaxByteOffset = [Capability.Addresses]
  requiredCapabilities _ = []
