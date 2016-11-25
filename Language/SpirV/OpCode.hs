{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.OpCode where

import           Data.Word (Word16)
import qualified Language.SpirV.Capability as Capability
import           Language.SpirV.SpirEnum

data OpCode =
    -- | This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpNop Source for OpNop>
    OpNop
    -- | Make an <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Intermediate intermediate> object whose value is undefined.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUndef Source for OpUndef>
  | OpUndef
    -- | Computes the run-time size of the type pointed to by /Pointer/
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSizeOf Source for OpSizeOf>
  | OpSizeOf
    -- | Continue specifying the /Source/ text from the previous instruction. This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSourceContinued Source for OpSourceContinued>
  | OpSourceContinued
    -- | Document what <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Source_Language source language> and text this module was translated from. This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSource Source for OpSource>
  | OpSource
    -- | Document an extension to the source language. This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSourceExtension Source for OpSourceExtension>
  | OpSourceExtension
    -- | Assign a name string to another instruction’s /Result/ . This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpName Source for OpName>
  | OpName
    -- | Assign a name string to a member of a structure type. This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemberName Source for OpMemberName>
  | OpMemberName
    -- | Assign a /Result/ to a string for use by other debug instructions (see __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLine OpLine>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSource OpSource>__). This has no semantic impact and can safely be removed from a module. (Removal also requires removal of all instructions referencing /Result/ .)
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpString Source for OpString>
  | OpString
    -- | Add source-level location information. This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLine Source for OpLine>
  | OpLine
    -- | Discontinue any source-level location information that might be active from a previous __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLine OpLine>__ instruction. This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpNoLine Source for OpNoLine>
  | OpNoLine
    -- | Document a process that was applied to a module. This has no semantic impact and can safely be removed from a module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpModuleProcessed Source for OpModuleProcessed>
  | OpModuleProcessed
    -- | Add a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decoration> to another /\<id>/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDecorate Source for OpDecorate>
  | OpDecorate
    -- | Add a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decoration> to a member of a structure type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemberDecorate Source for OpMemberDecorate>
  | OpMemberDecorate
    -- | A collector for <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decorations> from __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDecorate OpDecorate>__ instructions. All such __OpDecorate__ instructions targeting this __OpDecorationGroup__ instruction must precede it. Subsequent __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupDecorate OpGroupDecorate>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupMemberDecorate OpGroupMemberDecorate>__ instructions that consume this instruction’s /Result/ will apply these decorations to their targets.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDecorationGroup Source for OpDecorationGroup>
  | OpDecorationGroup
    -- | Add a group of <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decorations> to another /\<id>/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupDecorate Source for OpGroupDecorate>
  | OpGroupDecorate
    -- | Add a group of <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decorations> to members of structure types.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupMemberDecorate Source for OpGroupMemberDecorate>
  | OpGroupMemberDecorate
    -- | Declare use of an extension to SPIR-V. This allows validation of additional instructions, tokens, semantics, etc.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpExtension Source for OpExtension>
  | OpExtension
    -- | Import an extended set of instructions. It can be later referenced by the /Result/ .
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpExtInstImport Source for OpExtInstImport>
  | OpExtInstImport
    -- | Execute an instruction in an imported set of extended instructions.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpExtInst Source for OpExtInst>
  | OpExtInst
    -- | Set addressing model and memory model for the entire module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemoryModel Source for OpMemoryModel>
  | OpMemoryModel
    -- | Declare an <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#EntryPoint entry point> and its execution model.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEntryPoint Source for OpEntryPoint>
  | OpEntryPoint
    -- | Declare an execution mode for an entry point.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpExecutionMode Source for OpExecutionMode>
  | OpExecutionMode
    -- | Declare a capability used by this module.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCapability Source for OpCapability>
  | OpCapability
    -- | Declare the void type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeVoid Source for OpTypeVoid>
  | OpTypeVoid
    -- | Declare the /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Boolean Boolean type>/. Values of this type can only be either __true__ or __false__. There is no physical size or bit pattern defined for these values. If they are stored (in conjunction with __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVariable OpVariable>__), they can only be used with logical addressing operations, not physical, and only with non-externally visible shader <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Classes>: __Workgroup__, __CrossWorkgroup__, __Private__, and __Function__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeBool Source for OpTypeBool>
  | OpTypeBool
    -- | Declare a new /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Integer integer type>/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeInt Source for OpTypeInt>
  | OpTypeInt
    -- | Declare a new /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Floating floating-point type>/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeFloat Source for OpTypeFloat>
  | OpTypeFloat
    -- | Declare a new <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Vector vector type>.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeVector Source for OpTypeVector>
  | OpTypeVector
    -- | Declare a new matrix type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeMatrix Source for OpTypeMatrix>
  | OpTypeMatrix
    -- | Declare a new <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#ImageTerm image> type. Consumed, for example, by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeSampledImage OpTypeSampledImage>__. This type is opaque: values of this type have no defined physical size or bit pattern.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage Source for OpTypeImage>
  | OpTypeImage
    -- | Declare the <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler> type. Consumed by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSampledImage OpSampledImage>__. This type is opaque: values of this type have no defined physical size or bit pattern.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeSampler Source for OpTypeSampler>
  | OpTypeSampler
    -- | Declare a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#SampledImage sampled image> type, the /Result Type/ of __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSampledImage OpSampledImage>__, or an externally combined sampler and image. This type is opaque: values of this type have no defined physical size or bit pattern.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeSampledImage Source for OpTypeSampledImage>
  | OpTypeSampledImage
    -- | Declare a new array type: a dynamically-indexable ordered aggregate of elements all having the same type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeArray Source for OpTypeArray>
  | OpTypeArray
    -- | Declare a new run-time array type. Its length is not known at compile time.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeRuntimeArray Source for OpTypeRuntimeArray>
  | OpTypeRuntimeArray
    -- | Declare a new structure type: an aggregate of potentially heterogeneous members.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeStruct Source for OpTypeStruct>
  | OpTypeStruct
    -- | Declare a structure type with no body specified.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeOpaque Source for OpTypeOpaque>
  | OpTypeOpaque
    -- | Declare a new pointer type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypePointer Source for OpTypePointer>
  | OpTypePointer
    -- | Declare a new function type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeFunction Source for OpTypeFunction>
  | OpTypeFunction
    -- | Declare an OpenCL event type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeEvent Source for OpTypeEvent>
  | OpTypeEvent
    -- | Declare an OpenCL device-side event type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeDeviceEvent Source for OpTypeDeviceEvent>
  | OpTypeDeviceEvent
    -- | Declare an OpenCL reservation id type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeReserveId Source for OpTypeReserveId>
  | OpTypeReserveId
    -- | Declare an OpenCL queue type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeQueue Source for OpTypeQueue>
  | OpTypeQueue
    -- | Declare an OpenCL pipe type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypePipe Source for OpTypePipe>
  | OpTypePipe
    -- | Declare the Storage Class for a forward reference to a pointer.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeForwardPointer Source for OpTypeForwardPointer>
  | OpTypeForwardPointer
    -- | Declare the OpenCL pipe-storage type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypePipeStorage Source for OpTypePipeStorage>
  | OpTypePipeStorage
    -- | Declare the named-barrier type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeNamedBarrier Source for OpTypeNamedBarrier>
  | OpTypeNamedBarrier
    -- | Declare a __true__ /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Boolean Boolean-type>/ scalar constant.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantTrue Source for OpConstantTrue>
  | OpConstantTrue
    -- | Declare a __false__ /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Boolean Boolean-type>/ scalar constant.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantFalse Source for OpConstantFalse>
  | OpConstantFalse
    -- | Declare a new /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Integer integer-type>/ or /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Floating floating-point-type>/ scalar constant.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstant Source for OpConstant>
  | OpConstant
    -- | Declare a new /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#CompositeType composite>/ constant.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantComposite Source for OpConstantComposite>
  | OpConstantComposite
    -- | Declare a new sampler constant.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantSampler Source for OpConstantSampler>
  | OpConstantSampler
    -- | Declare a new /null/ constant value.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantNull Source for OpConstantNull>
  | OpConstantNull
    -- | Declare a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Boolean Boolean-type>/ scalar specialization constant with a default value of __true__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSpecConstantTrue Source for OpSpecConstantTrue>
  | OpSpecConstantTrue
    -- | Declare a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Boolean Boolean-type>/ scalar specialization constant with a default value of __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSpecConstantFalse Source for OpSpecConstantFalse>
  | OpSpecConstantFalse
    -- | Declare a new /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Integer integer-type>/ or /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Floating floating-point-type>/ scalar specialization constant.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSpecConstant Source for OpSpecConstant>
  | OpSpecConstant
    -- | Declare a new /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#CompositeType composite>/ specialization constant.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSpecConstantComposite Source for OpSpecConstantComposite>
  | OpSpecConstantComposite
    -- | Declare a new specialization constant that results from doing an operation.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSpecConstantOp Source for OpSpecConstantOp>
  | OpSpecConstantOp
    -- | Allocate an object in memory, resulting in a pointer to it, which can be used with __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLoad OpLoad>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpStore OpStore>__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVariable Source for OpVariable>
  | OpVariable
    -- | Form a pointer to a texel of an image. Use of such a pointer is limited to atomic operations.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageTexelPointer Source for OpImageTexelPointer>
  | OpImageTexelPointer
    -- | Load through a pointer.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLoad Source for OpLoad>
  | OpLoad
    -- | Store through a pointer.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpStore Source for OpStore>
  | OpStore
    -- | Copy from the memory pointed to by /Source/ to the memory pointed to by /Target/. Both operands must be non-void pointers of the same type. Matching Storage Class is not required. The amount of memory copied is the size of the type pointed to.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCopyMemory Source for OpCopyMemory>
  | OpCopyMemory
    -- | Copy from the memory pointed to by /Source/ to the memory pointed to by /Target/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCopyMemorySized Source for OpCopyMemorySized>
  | OpCopyMemorySized
    -- | Create a pointer into a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#CompositeType composite>/ object that can be used with __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLoad OpLoad>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpStore OpStore>__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAccessChain Source for OpAccessChain>
  | OpAccessChain
    -- | Has the same semantics as __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAccessChain OpAccessChain>__, with the addition that the resulting pointer is known to point within the base object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpInBoundsAccessChain Source for OpInBoundsAccessChain>
  | OpInBoundsAccessChain
    -- | Has the same semantics as __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAccessChain OpAccessChain>__, with the addition of the /Element/ operand.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpPtrAccessChain Source for OpPtrAccessChain>
  | OpPtrAccessChain
    -- | Length of a run-time array.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpArrayLength Source for OpArrayLength>
  | OpArrayLength
    -- | Result is a valid __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Memory_Semantics_-id- Memory Semantics>__ which includes mask bits set for the Storage Class for the specific (non-Generic) Storage Class of /Pointer/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGenericPtrMemSemantics Source for OpGenericPtrMemSemantics>
  | OpGenericPtrMemSemantics
    -- | Has the same semantics as __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpPtrAccessChain OpPtrAccessChain>__, with the addition that the resulting pointer is known to point within the base object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpInBoundsPtrAccessChain Source for OpInBoundsPtrAccessChain>
  | OpInBoundsPtrAccessChain
    -- | Add a function. This instruction must be immediately followed by one __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunctionParameter OpFunctionParameter>__ instruction per each formal parameter of this function. This function’s body or declaration will terminate with the next __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunctionEnd OpFunctionEnd>__ instruction.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunction Source for OpFunction>
  | OpFunction
    -- | Declare a formal parameter of the current function.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunctionParameter Source for OpFunctionParameter>
  | OpFunctionParameter
    -- | Last instruction of a function.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunctionEnd Source for OpFunctionEnd>
  | OpFunctionEnd
    -- | Call a function.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunctionCall Source for OpFunctionCall>
  | OpFunctionCall
    -- | Create a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#SampledImage sampled image>, containing both a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler> and an <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#ImageTerm image>.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSampledImage Source for OpSampledImage>
  | OpSampledImage
    -- | Sample an image with an implicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleImplicitLod Source for OpImageSampleImplicitLod>
  | OpImageSampleImplicitLod
    -- | Sample an image using an explicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleExplicitLod Source for OpImageSampleExplicitLod>
  | OpImageSampleExplicitLod
    -- | Sample an image doing depth-comparison with an implicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleDrefImplicitLod Source for OpImageSampleDrefImplicitLod>
  | OpImageSampleDrefImplicitLod
    -- | Sample an image doing depth-comparison using an explicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleDrefExplicitLod Source for OpImageSampleDrefExplicitLod>
  | OpImageSampleDrefExplicitLod
    -- | Sample an image with with a project coordinate and an implicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleProjImplicitLod Source for OpImageSampleProjImplicitLod>
  | OpImageSampleProjImplicitLod
    -- | Sample an image with a project coordinate using an explicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleProjExplicitLod Source for OpImageSampleProjExplicitLod>
  | OpImageSampleProjExplicitLod
    -- | Sample an image with a project coordinate, doing depth-comparison, with an implicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleProjDrefImplicitLod Source for OpImageSampleProjDrefImplicitLod>
  | OpImageSampleProjDrefImplicitLod
    -- | Sample an image with a project coordinate, doing depth-comparison, using an explicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSampleProjDrefExplicitLod Source for OpImageSampleProjDrefExplicitLod>
  | OpImageSampleProjDrefExplicitLod
    -- | Fetch a single texel from a sampled image.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageFetch Source for OpImageFetch>
  | OpImageFetch
    -- | Gathers the requested component from four texels.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageGather Source for OpImageGather>
  | OpImageGather
    -- | Gathers the requested depth-comparison from four texels.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageDrefGather Source for OpImageDrefGather>
  | OpImageDrefGather
    -- | Read a texel from an <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#ImageTerm image> without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageRead Source for OpImageRead>
  | OpImageRead
    -- | Write a texel to an <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#ImageTerm image> without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageWrite Source for OpImageWrite>
  | OpImageWrite
    -- | Extract the image from a sampled image.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImage Source for OpImage>
  | OpImage
    -- | Query the image format of an image created with an __Unknown__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Image_Format Image Format>.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageQueryFormat Source for OpImageQueryFormat>
  | OpImageQueryFormat
    -- | Query the channel order of an image created with an __Unknown__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Image_Format Image Format>.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageQueryOrder Source for OpImageQueryOrder>
  | OpImageQueryOrder
    -- | Query the dimensions of /Image/ for mipmap level for /Level of Detail/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageQuerySizeLod Source for OpImageQuerySizeLod>
  | OpImageQuerySizeLod
    -- | Query the dimensions of /Image/, with no level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageQuerySize Source for OpImageQuerySize>
  | OpImageQuerySize
    -- | Query the mipmap level and the level of detail for a hypothetical sampling of /Image/ at /Coordinate/ using an implicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageQueryLod Source for OpImageQueryLod>
  | OpImageQueryLod
    -- | Query the number of mipmap levels accessible through /Image/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageQueryLevels Source for OpImageQueryLevels>
  | OpImageQueryLevels
    -- | Query the number of samples available per texel fetch in a multisample image.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageQuerySamples Source for OpImageQuerySamples>
  | OpImageQuerySamples
    -- | Sample a sparse image with an implicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleImplicitLod Source for OpImageSparseSampleImplicitLod>
  | OpImageSparseSampleImplicitLod
    -- | Sample a sparse image using an explicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleExplicitLod Source for OpImageSparseSampleExplicitLod>
  | OpImageSparseSampleExplicitLod
    -- | Sample a sparse image doing depth-comparison with an implicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleDrefImplicitLod Source for OpImageSparseSampleDrefImplicitLod>
  | OpImageSparseSampleDrefImplicitLod
    -- | Sample a sparse image doing depth-comparison using an explicit level of detail.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleDrefExplicitLod Source for OpImageSparseSampleDrefExplicitLod>
  | OpImageSparseSampleDrefExplicitLod
    -- | Instruction reserved for future use. Use of this instruction is invalid.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleProjImplicitLod Source for OpImageSparseSampleProjImplicitLod>
  | OpImageSparseSampleProjImplicitLod
    -- | Instruction reserved for future use. Use of this instruction is invalid.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleProjExplicitLod Source for OpImageSparseSampleProjExplicitLod>
  | OpImageSparseSampleProjExplicitLod
    -- | Instruction reserved for future use. Use of this instruction is invalid.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleProjDrefImplicitLod Source for OpImageSparseSampleProjDrefImplicitLod>
  | OpImageSparseSampleProjDrefImplicitLod
    -- | Instruction reserved for future use. Use of this instruction is invalid.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseSampleProjDrefExplicitLod Source for OpImageSparseSampleProjDrefExplicitLod>
  | OpImageSparseSampleProjDrefExplicitLod
    -- | Fetch a single texel from a sampled sparse image.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseFetch Source for OpImageSparseFetch>
  | OpImageSparseFetch
    -- | Gathers the requested component from four texels of a sparse image.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseGather Source for OpImageSparseGather>
  | OpImageSparseGather
    -- | Gathers the requested depth-comparison from four texels of a sparse image.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseDrefGather Source for OpImageSparseDrefGather>
  | OpImageSparseDrefGather
    -- | Translates a /Resident Code/ into a Boolean. Result is __false__ if any of the texels were in uncommitted texture memory, and __true__ otherwise.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseTexelsResident Source for OpImageSparseTexelsResident>
  | OpImageSparseTexelsResident
    -- | Read a texel from a sparse <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#ImageTerm image> without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageSparseRead Source for OpImageSparseRead>
  | OpImageSparseRead
    -- | Convert (value preserving) from floating point to unsigned integer, with round toward 0.0.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConvertFToU Source for OpConvertFToU>
  | OpConvertFToU
    -- | Convert (value preserving) from floating point to signed integer, with round toward 0.0.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConvertFToS Source for OpConvertFToS>
  | OpConvertFToS
    -- | Convert (value preserving) from signed integer to floating point.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConvertSToF Source for OpConvertSToF>
  | OpConvertSToF
    -- | Convert (value preserving) from unsigned integer to floating point.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConvertUToF Source for OpConvertUToF>
  | OpConvertUToF
    -- | Convert (value preserving) unsigned width. This is either a truncate or a zero extend.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUConvert Source for OpUConvert>
  | OpUConvert
    -- | Convert (value preserving) signed width. This is either a truncate or a sign extend.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSConvert Source for OpSConvert>
  | OpSConvert
    -- | Convert (value preserving) floating-point width.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFConvert Source for OpFConvert>
  | OpFConvert
    -- | Quantize a floating-point value to what is expressible by a 16-bit floating-point value.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpQuantizeToF16 Source for OpQuantizeToF16>
  | OpQuantizeToF16
    -- | Convert a pointer to an unsigned integer type. A /Result Type/ width larger than the width of /Pointer/ will zero extend. A /Result Type/ smaller than the width of /Pointer/ will truncate. For same-width source and result, this is the same as __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitcast OpBitcast>__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConvertPtrToU Source for OpConvertPtrToU>
  | OpConvertPtrToU
    -- | Convert a signed integer to unsigned integer. Converted values outside the representable range of /Result Type/ are clamped to the nearest representable value of /Result Type/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSatConvertSToU Source for OpSatConvertSToU>
  | OpSatConvertSToU
    -- | Convert an unsigned integer to signed integer. Converted values outside the representable range of /Result Type/ are clamped to the nearest representable value of /Result Type/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSatConvertUToS Source for OpSatConvertUToS>
  | OpSatConvertUToS
    -- | Convert an integer to pointer. A /Result Type/ width smaller than the width of /Integer Value/ pointer will truncate. A /Result Type/ width larger than the width of /Integer Value/ pointer will zero extend.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConvertUToPtr Source for OpConvertUToPtr>
  | OpConvertUToPtr
    -- | Convert a pointer’s Storage Class to __Generic__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpPtrCastToGeneric Source for OpPtrCastToGeneric>
  | OpPtrCastToGeneric
    -- | Convert a pointer’s Storage Class to a non-__Generic__ class.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGenericCastToPtr Source for OpGenericCastToPtr>
  | OpGenericCastToPtr
    -- | Attempts to explicitly convert /Pointer/ to /Storage/ storage-class pointer value.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGenericCastToPtrExplicit Source for OpGenericCastToPtrExplicit>
  | OpGenericCastToPtrExplicit
    -- | Bit pattern-preserving type conversion.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitcast Source for OpBitcast>
  | OpBitcast
    -- | Extract a single, dynamically selected, component of a vector.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVectorExtractDynamic Source for OpVectorExtractDynamic>
  | OpVectorExtractDynamic
    -- | Make a copy of a vector, with a single, variably selected, component modified.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVectorInsertDynamic Source for OpVectorInsertDynamic>
  | OpVectorInsertDynamic
    -- | Select arbitrary components from two vectors to make a new vector.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVectorShuffle Source for OpVectorShuffle>
  | OpVectorShuffle
    -- | Construct a new /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#CompositeType composite>/ object from a set of constituent objects that will fully form it.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCompositeConstruct Source for OpCompositeConstruct>
  | OpCompositeConstruct
    -- | Extract a part of a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#CompositeType composite>/ object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCompositeExtract Source for OpCompositeExtract>
  | OpCompositeExtract
    -- | Make a copy of a /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#CompositeType composite>/ object, while modifying one part of it.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCompositeInsert Source for OpCompositeInsert>
  | OpCompositeInsert
    -- | Make a copy of /Operand/. There are no dereferences involved.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCopyObject Source for OpCopyObject>
  | OpCopyObject
    -- | Transpose a matrix.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTranspose Source for OpTranspose>
  | OpTranspose
    -- | Signed-integer subtract of /Operand/ from zero.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSNegate Source for OpSNegate>
  | OpSNegate
    -- | Floating-point subtract of /Operand/ from zero.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFNegate Source for OpFNegate>
  | OpFNegate
    -- | Integer addition of /Operand 1/ and /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIAdd Source for OpIAdd>
  | OpIAdd
    -- | Floating-point addition of /Operand 1/ and /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFAdd Source for OpFAdd>
  | OpFAdd
    -- | Integer subtraction of /Operand 2/ from /Operand 1/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpISub Source for OpISub>
  | OpISub
    -- | Floating-point subtraction of /Operand 2/ from /Operand 1/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFSub Source for OpFSub>
  | OpFSub
    -- | Integer multiplication of /Operand 1/ and /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIMul Source for OpIMul>
  | OpIMul
    -- | Floating-point multiplication of /Operand 1/ and /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFMul Source for OpFMul>
  | OpFMul
    -- | Unsigned-integer division of /Operand 1/ divided by /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUDiv Source for OpUDiv>
  | OpUDiv
    -- | Signed-integer division of /Operand 1/ divided by /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSDiv Source for OpSDiv>
  | OpSDiv
    -- | Floating-point division of /Operand 1/ divided by /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFDiv Source for OpFDiv>
  | OpFDiv
    -- | Unsigned modulo operation of /Operand 1/ modulo /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUMod Source for OpUMod>
  | OpUMod
    -- | Signed remainder operation of /Operand 1/ divided by /Operand 2/. The sign of a non-0 result comes from /Operand 1/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSRem Source for OpSRem>
  | OpSRem
    -- | Signed modulo operation of /Operand 1/ modulo /Operand 2/. The sign of a non-0 result comes from /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSMod Source for OpSMod>
  | OpSMod
    -- | Floating-point remainder operation of /Operand 1/ divided by /Operand 2/. The sign of a non-0 result comes from /Operand 1/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFRem Source for OpFRem>
  | OpFRem
    -- | Floating-point remainder operation of /Operand 1/ divided by /Operand 2/. The sign of a non-0 result comes from /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFMod Source for OpFMod>
  | OpFMod
    -- | Scale a floating-point vector.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVectorTimesScalar Source for OpVectorTimesScalar>
  | OpVectorTimesScalar
    -- | Scale a floating-point matrix.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMatrixTimesScalar Source for OpMatrixTimesScalar>
  | OpMatrixTimesScalar
    -- | Linear-algebraic /Vector X Matrix/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpVectorTimesMatrix Source for OpVectorTimesMatrix>
  | OpVectorTimesMatrix
    -- | Linear-algebraic /Vector X Matrix/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMatrixTimesVector Source for OpMatrixTimesVector>
  | OpMatrixTimesVector
    -- | Linear-algebraic multiply of /LeftMatrix/ X /RightMatrix/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMatrixTimesMatrix Source for OpMatrixTimesMatrix>
  | OpMatrixTimesMatrix
    -- | Linear-algebraic outer product of /Vector 1/ and /Vector 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpOuterProduct Source for OpOuterProduct>
  | OpOuterProduct
    -- | Dot product of /Vector 1/ and /Vector 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDot Source for OpDot>
  | OpDot
    -- | Result is the unsigned integer addition of /Operand 1/ and /Operand 2/, including its carry.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIAddCarry Source for OpIAddCarry>
  | OpIAddCarry
    -- | Result is the unsigned integer subtraction of /Operand 2/ from /Operand 1/, and what it needed to borrow.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpISubBorrow Source for OpISubBorrow>
  | OpISubBorrow
    -- | Result is the full value of the unsigned integer multiplication of /Operand 1/ and /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUMulExtended Source for OpUMulExtended>
  | OpUMulExtended
    -- | Result is the full value of the signed integer multiplication of /Operand 1/ and /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSMulExtended Source for OpSMulExtended>
  | OpSMulExtended
    -- | Shift the bits in /Base/ right by the number of bits specified in /Shift/. The most-significant bits will be zero filled.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpShiftRightLogical Source for OpShiftRightLogical>
  | OpShiftRightLogical
    -- | Shift the bits in /Base/ right by the number of bits specified in /Shift/. The most-significant bits will be filled with the sign bit from /Base/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpShiftRightArithmetic Source for OpShiftRightArithmetic>
  | OpShiftRightArithmetic
    -- | Shift the bits in /Base/ left by the number of bits specified in /Shift/. The least-significant bits will be zero filled.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpShiftLeftLogical Source for OpShiftLeftLogical>
  | OpShiftLeftLogical
    -- | Result is 1 if either /Operand 1/ or /Operand 2/ is 1. Result is 0 if both /Operand 1/ and /Operand 2/ are 0.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitwiseOr Source for OpBitwiseOr>
  | OpBitwiseOr
    -- | Result is 1 if exactly one of /Operand 1/ or /Operand 2/ is 1. Result is 0 if /Operand 1/ and /Operand 2/ have the same value.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitwiseXor Source for OpBitwiseXor>
  | OpBitwiseXor
    -- | Result is 1 if both /Operand 1/ and /Operand 2/ are 1. Result is 0 if either /Operand 1/ or /Operand 2/ are 0.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitwiseAnd Source for OpBitwiseAnd>
  | OpBitwiseAnd
    -- | Complement the bits of /Operand/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpNot Source for OpNot>
  | OpNot
    -- | Make a copy of an object, with a modified bit field that comes from another object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitFieldInsert Source for OpBitFieldInsert>
  | OpBitFieldInsert
    -- | Extract a bit field from an object, with sign extension.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitFieldSExtract Source for OpBitFieldSExtract>
  | OpBitFieldSExtract
    -- | Extract a bit field from an object, without sign extension.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitFieldUExtract Source for OpBitFieldUExtract>
  | OpBitFieldUExtract
    -- | Reverse the bits in an object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitReverse Source for OpBitReverse>
  | OpBitReverse
    -- | Count the number of set bits in an object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBitCount Source for OpBitCount>
  | OpBitCount
    -- | Result is __true__ if any component of /Vector/ is __true__, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAny Source for OpAny>
  | OpAny
    -- | Result is __true__ if all components of /Vector/ are __true__, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAll Source for OpAll>
  | OpAll
    -- | Result is __true__ if /x/ is an IEEE NaN, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIsNan Source for OpIsNan>
  | OpIsNan
    -- | Result is __true__ if /x/ is an IEEE Inf, otherwise result is __false__
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIsInf Source for OpIsInf>
  | OpIsInf
    -- | Result is __true__ if /x/ is an IEEE finite number, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIsFinite Source for OpIsFinite>
  | OpIsFinite
    -- | Result is __true__ if /x/ is an IEEE normal number, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIsNormal Source for OpIsNormal>
  | OpIsNormal
    -- | Result is __true__ if /x/ has its sign bit set, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSignBitSet Source for OpSignBitSet>
  | OpSignBitSet
    -- | Result is __true__ if /x/ \< /y/ or /x/ > /y/, where IEEE comparisons are used, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLessOrGreater Source for OpLessOrGreater>
  | OpLessOrGreater
    -- | Result is __true__ if both /x/ == /x/ and /y/ == /y/ are __true__, where IEEE comparison is used, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpOrdered Source for OpOrdered>
  | OpOrdered
    -- | Result is __true__ if either /x/ or /y/ is an IEEE NaN, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUnordered Source for OpUnordered>
  | OpUnordered
    -- | Result is __true__ if /Operand 1/ and /Operand 2/ have the same value. Result is __false__ if /Operand 1/ and /Operand 2/ have different values.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLogicalEqual Source for OpLogicalEqual>
  | OpLogicalEqual
    -- | Result is __true__ if /Operand 1/ and /Operand 2/ have different values. Result is __false__ if /Operand 1/ and /Operand 2/ have the same value.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLogicalNotEqual Source for OpLogicalNotEqual>
  | OpLogicalNotEqual
    -- | Result is __true__ if either /Operand 1/ or /Operand 2/ is __true__. Result is __false__ if both /Operand 1/ and /Operand 2/ are __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLogicalOr Source for OpLogicalOr>
  | OpLogicalOr
    -- | Result is __true__ if both /Operand 1/ and /Operand 2/ are __true__. Result is __false__ if either /Operand 1/ or /Operand 2/ are __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLogicalAnd Source for OpLogicalAnd>
  | OpLogicalAnd
    -- | Result is __true__ if /Operand/ is __false__. Result is __false__ if /Operand/ is __true__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLogicalNot Source for OpLogicalNot>
  | OpLogicalNot
    -- | Select between two objects.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSelect Source for OpSelect>
  | OpSelect
    -- | Integer comparison for equality.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIEqual Source for OpIEqual>
  | OpIEqual
    -- | Integer comparison for inequality.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpINotEqual Source for OpINotEqual>
  | OpINotEqual
    -- | Unsigned-integer comparison if /Operand 1/ is greater than /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUGreaterThan Source for OpUGreaterThan>
  | OpUGreaterThan
    -- | Signed-integer comparison if /Operand 1/ is greater than /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSGreaterThan Source for OpSGreaterThan>
  | OpSGreaterThan
    -- | Unsigned-integer comparison if /Operand 1/ is greater than or equal to /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUGreaterThanEqual Source for OpUGreaterThanEqual>
  | OpUGreaterThanEqual
    -- | Signed-integer comparison if /Operand 1/ is greater than or equal to /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSGreaterThanEqual Source for OpSGreaterThanEqual>
  | OpSGreaterThanEqual
    -- | Unsigned-integer comparison if /Operand 1/ is less than /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpULessThan Source for OpULessThan>
  | OpULessThan
    -- | Signed-integer comparison if /Operand 1/ is less than /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSLessThan Source for OpSLessThan>
  | OpSLessThan
    -- | Unsigned-integer comparison if /Operand 1/ is less than or equal to /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpULessThanEqual Source for OpULessThanEqual>
  | OpULessThanEqual
    -- | Signed-integer comparison if /Operand 1/ is less than or equal to /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSLessThanEqual Source for OpSLessThanEqual>
  | OpSLessThanEqual
    -- | Floating-point comparison for being ordered and equal.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFOrdEqual Source for OpFOrdEqual>
  | OpFOrdEqual
    -- | Floating-point comparison for being unordered or equal.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFUnordEqual Source for OpFUnordEqual>
  | OpFUnordEqual
    -- | Floating-point comparison for being ordered and not equal.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFOrdNotEqual Source for OpFOrdNotEqual>
  | OpFOrdNotEqual
    -- | Floating-point comparison for being unordered or not equal.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFUnordNotEqual Source for OpFUnordNotEqual>
  | OpFUnordNotEqual
    -- | Floating-point comparison if operands are ordered and /Operand 1/ is less than /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFOrdLessThan Source for OpFOrdLessThan>
  | OpFOrdLessThan
    -- | Floating-point comparison if operands are unordered or /Operand 1/ is less than /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFUnordLessThan Source for OpFUnordLessThan>
  | OpFUnordLessThan
    -- | Floating-point comparison if operands are ordered and /Operand 1/ is greater than /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFOrdGreaterThan Source for OpFOrdGreaterThan>
  | OpFOrdGreaterThan
    -- | Floating-point comparison if operands are unordered or /Operand 1/ is greater than /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFUnordGreaterThan Source for OpFUnordGreaterThan>
  | OpFUnordGreaterThan
    -- | Floating-point comparison if operands are ordered and /Operand 1/ is less than or equal to /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFOrdLessThanEqual Source for OpFOrdLessThanEqual>
  | OpFOrdLessThanEqual
    -- | Floating-point comparison if operands are unordered or /Operand 1/ is less than or equal to /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFUnordLessThanEqual Source for OpFUnordLessThanEqual>
  | OpFUnordLessThanEqual
    -- | Floating-point comparison if operands are ordered and /Operand 1/ is greater than or equal to /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFOrdGreaterThanEqual Source for OpFOrdGreaterThanEqual>
  | OpFOrdGreaterThanEqual
    -- | Floating-point comparison if operands are unordered or /Operand 1/ is greater than or equal to /Operand 2/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFUnordGreaterThanEqual Source for OpFUnordGreaterThanEqual>
  | OpFUnordGreaterThanEqual
    -- | Same result as either __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdxFine OpDPdxFine>__ or __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdxCoarse OpDPdxCoarse>__ on /P/. Selection of which one is based on external factors.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdx Source for OpDPdx>
  | OpDPdx
    -- | Same result as either __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdyFine OpDPdyFine>__ or __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdyCoarse OpDPdyCoarse>__ on /P/. Selection of which one is based on external factors.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdy Source for OpDPdy>
  | OpDPdy
    -- | Result is the same as computing the sum of the absolute values of __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdx OpDPdx>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdy OpDPdy>__ on /P/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFwidth Source for OpFwidth>
  | OpFwidth
    -- | Result is the partial derivative of /P/ with respect to the window /x/ coordinate.Will use local differencing based on the value of /P/ for the current fragment and its immediate neighbor(s).
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdxFine Source for OpDPdxFine>
  | OpDPdxFine
    -- | Result is the partial derivative of /P/ with respect to the window /y/ coordinate.Will use local differencing based on the value of /P/ for the current fragment and its immediate neighbor(s).
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdyFine Source for OpDPdyFine>
  | OpDPdyFine
    -- | Result is the same as computing the sum of the absolute values of __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdxFine OpDPdxFine>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdyFine OpDPdyFine>__ on /P/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFwidthFine Source for OpFwidthFine>
  | OpFwidthFine
    -- | Result is the partial derivative of /P/ with respect to the window /x/ coordinate. Will use local differencing based on the value of /P/ for the current fragment’s neighbors, and will possibly, but not necessarily, include the value of /P/ for the current fragment. That is, over a given area, the implementation can compute /x/ derivatives in fewer unique locations than would be allowed for __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdxFine OpDPdxFine>__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdxCoarse Source for OpDPdxCoarse>
  | OpDPdxCoarse
    -- | Result is the partial derivative of /P/ with respect to the window /y/ coordinate. Will use local differencing based on the value of /P/ for the current fragment’s neighbors, and will possibly, but not necessarily, include the value of /P/ for the current fragment. That is, over a given area, the implementation can compute /y/ derivatives in fewer unique locations than would be allowed for __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdyFine OpDPdyFine>__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdyCoarse Source for OpDPdyCoarse>
  | OpDPdyCoarse
    -- | Result is the same as computing the sum of the absolute values of __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdxCoarse OpDPdxCoarse>__ and __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdyCoarse OpDPdyCoarse>__ on /P/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFwidthCoarse Source for OpFwidthCoarse>
  | OpFwidthCoarse
    -- | The SSA phi function.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpPhi Source for OpPhi>
  | OpPhi
    -- | Declare a structured loop.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLoopMerge Source for OpLoopMerge>
  | OpLoopMerge
    -- | Declare a structured selection.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSelectionMerge Source for OpSelectionMerge>
  | OpSelectionMerge
    -- | The block label instruction: Any reference to a block is through the /Result/ of its label.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLabel Source for OpLabel>
  | OpLabel
    -- | Unconditional branch to /Target Label/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBranch Source for OpBranch>
  | OpBranch
    -- | If /Condition/ is __true__, branch to /True Label/, otherwise branch to /False Label/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBranchConditional Source for OpBranchConditional>
  | OpBranchConditional
    -- | Multi-way branch to one of the operand label /\<id>/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSwitch Source for OpSwitch>
  | OpSwitch
    -- | Fragment-shader discard.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpKill Source for OpKill>
  | OpKill
    -- | Return with no value from a function with void return type.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpReturn Source for OpReturn>
  | OpReturn
    -- | Return a value from a function.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpReturnValue Source for OpReturnValue>
  | OpReturnValue
    -- | Declares that this block is not reachable in the CFG.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpUnreachable Source for OpUnreachable>
  | OpUnreachable
    -- | Declare that an object was not defined before this instruction.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLifetimeStart Source for OpLifetimeStart>
  | OpLifetimeStart
    -- | Declare that an object is dead after this instruction.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLifetimeStop Source for OpLifetimeStop>
  | OpLifetimeStop
    -- | Atomically load through /Pointer/ using the given /Semantics/. All subparts of the value that is loaded will be read atomically with respect to all other atomic accesses to it within /Scope/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicLoad Source for OpAtomicLoad>
  | OpAtomicLoad
    -- | Atomically store through /Pointer/ using the given /Semantics/. All subparts of /Value/ will be written atomically with respect to all other atomic accesses to it within /Scope/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicStore Source for OpAtomicStore>
  | OpAtomicStore
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicExchange Source for OpAtomicExchange>
  | OpAtomicExchange
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicCompareExchange Source for OpAtomicCompareExchange>
  | OpAtomicCompareExchange
    -- | Attempts to do the following:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicCompareExchangeWeak Source for OpAtomicCompareExchangeWeak>
  | OpAtomicCompareExchangeWeak
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicIIncrement Source for OpAtomicIIncrement>
  | OpAtomicIIncrement
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicIDecrement Source for OpAtomicIDecrement>
  | OpAtomicIDecrement
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicIAdd Source for OpAtomicIAdd>
  | OpAtomicIAdd
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicISub Source for OpAtomicISub>
  | OpAtomicISub
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicSMin Source for OpAtomicSMin>
  | OpAtomicSMin
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicUMin Source for OpAtomicUMin>
  | OpAtomicUMin
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicSMax Source for OpAtomicSMax>
  | OpAtomicSMax
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicUMax Source for OpAtomicUMax>
  | OpAtomicUMax
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicAnd Source for OpAtomicAnd>
  | OpAtomicAnd
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicOr Source for OpAtomicOr>
  | OpAtomicOr
    -- | Perform the following steps atomically with respect to any other atomic accesses within /Scope/ to the same location:
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicXor Source for OpAtomicXor>
  | OpAtomicXor
    -- | Atomically sets the flag value pointed to by /Pointer/ to the set state.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicFlagTestAndSet Source for OpAtomicFlagTestAndSet>
  | OpAtomicFlagTestAndSet
    -- | Atomically sets the flag value pointed to by /Pointer/ to the clear state.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpAtomicFlagClear Source for OpAtomicFlagClear>
  | OpAtomicFlagClear
    -- | Emits the current values of all output variables to the current output primitive. After execution, the values of all output variables are undefined.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEmitVertex Source for OpEmitVertex>
  | OpEmitVertex
    -- | Finish the current primitive and start a new one. No vertex is emitted.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEndPrimitive Source for OpEndPrimitive>
  | OpEndPrimitive
    -- | Emits the current values of all output variables to the current output primitive. After execution, the values of all output variables are undefined.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEmitStreamVertex Source for OpEmitStreamVertex>
  | OpEmitStreamVertex
    -- | Finish the current primitive and start a new one. No vertex is emitted.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEndStreamPrimitive Source for OpEndStreamPrimitive>
  | OpEndStreamPrimitive
    -- | Wait for other invocations of this module to reach the current point of execution.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpControlBarrier Source for OpControlBarrier>
  | OpControlBarrier
    -- | Control the order that memory accesses are observed.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemoryBarrier Source for OpMemoryBarrier>
  | OpMemoryBarrier
    -- | Declare a new named-barrier object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpNamedBarrierInitialize Source for OpNamedBarrierInitialize>
  | OpNamedBarrierInitialize
    -- | Wait for other invocations of this module to reach the current point of execution.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpMemoryNamedBarrier Source for OpMemoryNamedBarrier>
  | OpMemoryNamedBarrier
    -- | Perform an asynchronous group copy of /Num Elements/ elements from /Source/ to /Destination/. The asynchronous copy is performed by all work-items in a group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupAsyncCopy Source for OpGroupAsyncCopy>
  | OpGroupAsyncCopy
    -- | Wait for events generated by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupAsyncCopy OpGroupAsyncCopy>__ operations to complete. /Events List/ points to /Num Events/ event objects, which will be released after the wait is performed.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupWaitEvents Source for OpGroupWaitEvents>
  | OpGroupWaitEvents
    -- | Evaluates a predicate for all invocations in the group,resulting in __true__ if predicate evaluates to __true__ for all <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group, otherwise the result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupAll Source for OpGroupAll>
  | OpGroupAll
    -- | Evaluates a predicate for all invocations in the group,resulting in __true__ if predicate evaluates to __true__ for any <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation> in the group, otherwise the result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupAny Source for OpGroupAny>
  | OpGroupAny
    -- | Return the /Value/ of the <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation> identified by the local id /LocalId/ to all invocations in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupBroadcast Source for OpGroupBroadcast>
  | OpGroupBroadcast
    -- | An integer add group operation specified for all values of /X/ specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupIAdd Source for OpGroupIAdd>
  | OpGroupIAdd
    -- | A floating-point add group operation specified for all values of /X/ specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFAdd Source for OpGroupFAdd>
  | OpGroupFAdd
    -- | A floating-point minimum group operation specified for all values of /X/ specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFMin Source for OpGroupFMin>
  | OpGroupFMin
    -- | An unsigned integer minimum group operation specified for all values of /X/ specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupUMin Source for OpGroupUMin>
  | OpGroupUMin
    -- | A signed integer minimum group operation specified for all values of /X/ specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupSMin Source for OpGroupSMin>
  | OpGroupSMin
    -- | A floating-point maximum group operation specified for all values of /X/ specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFMax Source for OpGroupFMax>
  | OpGroupFMax
    -- | An unsigned integer maximum group operation specified for all values of /X/ specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupUMax Source for OpGroupUMax>
  | OpGroupUMax
    -- | A signed integer maximum group operation specified for all values of /X/ specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> in the group.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupSMax Source for OpGroupSMax>
  | OpGroupSMax
    -- | See extension SPV_KHR_shader_ballot
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSubgroupBallotKHR Source for OpSubgroupBallotKHR>
  | OpSubgroupBallotKHR
    -- | See extension SPV_KHR_shader_ballot
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSubgroupFirstInvocationKHR Source for OpSubgroupFirstInvocationKHR>
  | OpSubgroupFirstInvocationKHR
    -- | Enqueue a marker command to the queue object specified by /Queue/. The marker command waits for a list of events to complete, or if the list is empty it waits for all previously enqueued commands in /Queue/ to complete before the marker completes.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEnqueueMarker Source for OpEnqueueMarker>
  | OpEnqueueMarker
    -- | Enqueue the function specified by /Invoke/ and the NDRange specified by /ND Range/ for execution to the queue object specified by /Queue/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEnqueueKernel Source for OpEnqueueKernel>
  | OpEnqueueKernel
    -- | Returns the number of subgroups in each workgroup of the dispatch (except for the last in cases where the global size does not divide cleanly into work-groups) given the combination of the passed NDRange descriptor specified by /ND Range/ and the function specified by /Invoke/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetKernelNDrangeSubGroupCount Source for OpGetKernelNDrangeSubGroupCount>
  | OpGetKernelNDrangeSubGroupCount
    -- | Returns the maximum sub-group size for the function specified by /Invoke/ and the NDRange specified by /ND Range/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetKernelNDrangeMaxSubGroupSize Source for OpGetKernelNDrangeMaxSubGroupSize>
  | OpGetKernelNDrangeMaxSubGroupSize
    -- | Returns the maximum work-group size that can be used to execute the function specified by /Invoke/ on the device.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetKernelWorkGroupSize Source for OpGetKernelWorkGroupSize>
  | OpGetKernelWorkGroupSize
    -- | Returns the preferred multiple of work-group size for the function specified by /Invoke/. This is a performance hint. Specifying a work-group size that is not a multiple of the value returned by this query as the value of the local work size will not fail to enqueue /Invoke/ for execution unless the work-group size specified is larger than the device maximum.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetKernelPreferredWorkGroupSizeMultiple Source for OpGetKernelPreferredWorkGroupSizeMultiple>
  | OpGetKernelPreferredWorkGroupSizeMultiple
    -- | Increments the reference count of the event object specified by /Event/.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpRetainEvent Source for OpRetainEvent>
  | OpRetainEvent
    -- | Decrements the reference count of the event object specified by /Event/. The event object is deleted once the event reference count is zero, the specific command identified by this event has completed (or terminated) and there are no commands in any device command queue that require a wait for this event to complete.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpReleaseEvent Source for OpReleaseEvent>
  | OpReleaseEvent
    -- | Create a user event. The execution status of the created event is set to a value of 2 (CL_SUBMITTED).
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCreateUserEvent Source for OpCreateUserEvent>
  | OpCreateUserEvent
    -- | Returns __true__ if the event specified by /Event/ is a valid event, otherwise result is __false__.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIsValidEvent Source for OpIsValidEvent>
  | OpIsValidEvent
    -- | Sets the execution status of a user event specified by /Event/./Status/ can be either 0 (CL_COMPLETE) to indicate that this kernel and all its child kernels finished execution successfully, or a negative integer value indicating an error.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSetUserEventStatus Source for OpSetUserEventStatus>
  | OpSetUserEventStatus
    -- | Captures the profiling information specified by /Profiling Info/ for the command associated with the event specified by /Event/ in the memory pointed to by /Value/.The profiling information will be available in the memory pointed to by /Value/ once the command identified by /Event/ has completed.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCaptureEventProfilingInfo Source for OpCaptureEventProfilingInfo>
  | OpCaptureEventProfilingInfo
    -- | Returns the default device queue. If a default device queue has not been created, a null queue object is returned.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetDefaultQueue Source for OpGetDefaultQueue>
  | OpGetDefaultQueue
    -- | Given the global work size specified by /GlobalWorkSize/, local work size specified by /LocalWorkSize/ and global work offset specified by /GlobalWorkOffset/, builds a 1D, 2D or 3D ND-range descriptor structure and returns it.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpBuildNDRange Source for OpBuildNDRange>
  | OpBuildNDRange
    -- | Returns the 1D local size to enqueue /Invoke/ with /Subgroup Count/ subgroups per workgroup.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetKernelLocalSizeForSubgroupCount Source for OpGetKernelLocalSizeForSubgroupCount>
  | OpGetKernelLocalSizeForSubgroupCount
    -- | Returns the maximum number of subgroups that can be used to execute /Invoke/ on the devce.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetKernelMaxNumSubgroups Source for OpGetKernelMaxNumSubgroups>
  | OpGetKernelMaxNumSubgroups
    -- | Read a packet from the pipe object specified by /Pipe/ into /Pointer/. Result is 0 if the operation is successful and a negative value if the pipe is empty.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpReadPipe Source for OpReadPipe>
  | OpReadPipe
    -- | Write a packet from /Pointer/ to the pipe object specified by /Pipe/. Result is 0 if the operation is successful and a negative value if the pipe is full.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpWritePipe Source for OpWritePipe>
  | OpWritePipe
    -- | Read a packet from the reserved area specified by /Reserve Id/ and /Index/ of the pipe object specified by /Pipe/ into /Pointer/. The reserved pipe entries are referred to by indices that go from 0 … /Num Packets/ - 1. Result is 0 if the operation is successful and a negative value otherwise.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpReservedReadPipe Source for OpReservedReadPipe>
  | OpReservedReadPipe
    -- | Write a packet from /Pointer/ into the reserved area specified by /Reserve Id/ and /Index/ of the pipe object specified by /Pipe/. The reserved pipe entries are referred to by indices that go from 0 … /Num Packets/ - 1. Result is 0 if the operation is successful and a negative value otherwise.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpReservedWritePipe Source for OpReservedWritePipe>
  | OpReservedWritePipe
    -- | Reserve /Num Packets/ entries for reading from the pipe object specified by /Pipe/. Result is a valid reservation ID if the reservation is successful.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpReserveReadPipePackets Source for OpReserveReadPipePackets>
  | OpReserveReadPipePackets
    -- | Reserve /num_packets/ entries for writing to the pipe object specified by /Pipe/. Result is a valid reservation ID if the reservation is successful.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpReserveWritePipePackets Source for OpReserveWritePipePackets>
  | OpReserveWritePipePackets
    -- | Indicates that all reads to /Num Packets/ associated with the reservation specified by /Reserve Id/ and the pipe object specified by /Pipe/ are completed.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCommitReadPipe Source for OpCommitReadPipe>
  | OpCommitReadPipe
    -- | Indicates that all writes to /Num Packets/ associated with the reservation specified by /Reserve Id/ and the pipe object specified by /Pipe/ are completed.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCommitWritePipe Source for OpCommitWritePipe>
  | OpCommitWritePipe
    -- | Return __true__ if /Reserve Id/ is a valid reservation id and __false__ otherwise.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpIsValidReserveId Source for OpIsValidReserveId>
  | OpIsValidReserveId
    -- | Result is the number of available entries in the pipe object specified by /Pipe/. The number of available entries in a pipe is a dynamic value. The value returned should be considered immediately stale.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetNumPipePackets Source for OpGetNumPipePackets>
  | OpGetNumPipePackets
    -- | Result is the maximum number of packets specified when the pipe object specified by /Pipe/ was created.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGetMaxPipePackets Source for OpGetMaxPipePackets>
  | OpGetMaxPipePackets
    -- | Reserve /Num Packets/ entries for reading from the pipe object specified by /Pipe/ at group level. Result is a valid reservation id if the reservation is successful.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupReserveReadPipePackets Source for OpGroupReserveReadPipePackets>
  | OpGroupReserveReadPipePackets
    -- | Reserve /Num Packets/ entries for writing to the pipe object specified by /Pipe/ at group level. Result is a valid reservation ID if the reservation is successful.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupReserveWritePipePackets Source for OpGroupReserveWritePipePackets>
  | OpGroupReserveWritePipePackets
    -- | A group level indication that all reads to /Num Packets/ associated with the reservation specified by /Reserve Id/ to the pipe object specified by /Pipe/ are completed.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupCommitReadPipe Source for OpGroupCommitReadPipe>
  | OpGroupCommitReadPipe
    -- | A group level indication that all writes to /Num Packets/ associated with the reservation specified by /Reserve Id/ to the pipe object specified by /Pipe/ are completed.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupCommitWritePipe Source for OpGroupCommitWritePipe>
  | OpGroupCommitWritePipe
    -- | Creates a pipe-storage object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantPipeStorage Source for OpConstantPipeStorage>
  | OpConstantPipeStorage
    -- | Creates a pipe object from a pipe-storage object.
    --
    -- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCreatePipeFromPipeStorage Source for OpCreatePipeFromPipeStorage>
  | OpCreatePipeFromPipeStorage
  deriving(Read, Show, Eq, Ord)

data InstructionSize = Fixed Word16
                     | Variadic Word16
  deriving(Read, Show, Eq)

instructionSize :: OpCode -> InstructionSize
instructionSize OpNop = Fixed 1
instructionSize OpUndef = Fixed 3
instructionSize OpSizeOf = Fixed 4
instructionSize OpSourceContinued = Variadic 2
instructionSize OpSource = Variadic 3
instructionSize OpSourceExtension = Variadic 2
instructionSize OpName = Variadic 3
instructionSize OpMemberName = Variadic 4
instructionSize OpString = Variadic 3
instructionSize OpLine = Fixed 4
instructionSize OpNoLine = Fixed 1
instructionSize OpModuleProcessed = Variadic 2
instructionSize OpDecorate = Variadic 3
instructionSize OpMemberDecorate = Variadic 4
instructionSize OpDecorationGroup = Fixed 2
instructionSize OpGroupDecorate = Variadic 2
instructionSize OpGroupMemberDecorate = Fixed 2
instructionSize OpExtension = Variadic 2
instructionSize OpExtInstImport = Variadic 3
instructionSize OpExtInst = Variadic 5
instructionSize OpMemoryModel = Fixed 3
instructionSize OpEntryPoint = Variadic 4
instructionSize OpExecutionMode = Variadic 3
instructionSize OpCapability = Fixed 2
instructionSize OpTypeVoid = Fixed 2
instructionSize OpTypeBool = Fixed 2
instructionSize OpTypeInt = Fixed 4
instructionSize OpTypeFloat = Fixed 3
instructionSize OpTypeVector = Fixed 4
instructionSize OpTypeMatrix = Fixed 4
instructionSize OpTypeImage = Variadic 9
instructionSize OpTypeSampler = Fixed 2
instructionSize OpTypeSampledImage = Fixed 3
instructionSize OpTypeArray = Fixed 4
instructionSize OpTypeRuntimeArray = Fixed 3
instructionSize OpTypeStruct = Variadic 2
instructionSize OpTypeOpaque = Variadic 3
instructionSize OpTypePointer = Fixed 4
instructionSize OpTypeFunction = Variadic 3
instructionSize OpTypeEvent = Fixed 2
instructionSize OpTypeDeviceEvent = Fixed 2
instructionSize OpTypeReserveId = Fixed 2
instructionSize OpTypeQueue = Fixed 2
instructionSize OpTypePipe = Fixed 3
instructionSize OpTypeForwardPointer = Fixed 3
instructionSize OpTypePipeStorage = Fixed 2
instructionSize OpTypeNamedBarrier = Fixed 2
instructionSize OpConstantTrue = Fixed 3
instructionSize OpConstantFalse = Fixed 3
instructionSize OpConstant = Variadic 3
instructionSize OpConstantComposite = Variadic 3
instructionSize OpConstantSampler = Fixed 6
instructionSize OpConstantNull = Fixed 3
instructionSize OpSpecConstantTrue = Fixed 3
instructionSize OpSpecConstantFalse = Fixed 3
instructionSize OpSpecConstant = Variadic 3
instructionSize OpSpecConstantComposite = Variadic 3
instructionSize OpSpecConstantOp = Variadic 4
instructionSize OpVariable = Variadic 4
instructionSize OpImageTexelPointer = Fixed 6
instructionSize OpLoad = Variadic 4
instructionSize OpStore = Variadic 3
instructionSize OpCopyMemory = Variadic 3
instructionSize OpCopyMemorySized = Variadic 4
instructionSize OpAccessChain = Variadic 4
instructionSize OpInBoundsAccessChain = Variadic 4
instructionSize OpPtrAccessChain = Variadic 5
instructionSize OpArrayLength = Fixed 5
instructionSize OpGenericPtrMemSemantics = Fixed 4
instructionSize OpInBoundsPtrAccessChain = Variadic 5
instructionSize OpFunction = Fixed 5
instructionSize OpFunctionParameter = Fixed 3
instructionSize OpFunctionEnd = Fixed 1
instructionSize OpFunctionCall = Variadic 4
instructionSize OpSampledImage = Fixed 5
instructionSize OpImageSampleImplicitLod = Variadic 5
instructionSize OpImageSampleExplicitLod = Variadic 7
instructionSize OpImageSampleDrefImplicitLod = Variadic 6
instructionSize OpImageSampleDrefExplicitLod = Variadic 8
instructionSize OpImageSampleProjImplicitLod = Variadic 5
instructionSize OpImageSampleProjExplicitLod = Variadic 7
instructionSize OpImageSampleProjDrefImplicitLod = Variadic 6
instructionSize OpImageSampleProjDrefExplicitLod = Variadic 8
instructionSize OpImageFetch = Variadic 5
instructionSize OpImageGather = Variadic 6
instructionSize OpImageDrefGather = Variadic 6
instructionSize OpImageRead = Variadic 5
instructionSize OpImageWrite = Variadic 4
instructionSize OpImage = Fixed 4
instructionSize OpImageQueryFormat = Fixed 4
instructionSize OpImageQueryOrder = Fixed 4
instructionSize OpImageQuerySizeLod = Fixed 5
instructionSize OpImageQuerySize = Fixed 4
instructionSize OpImageQueryLod = Fixed 5
instructionSize OpImageQueryLevels = Fixed 4
instructionSize OpImageQuerySamples = Fixed 4
instructionSize OpImageSparseSampleImplicitLod = Variadic 5
instructionSize OpImageSparseSampleExplicitLod = Variadic 7
instructionSize OpImageSparseSampleDrefImplicitLod = Variadic 6
instructionSize OpImageSparseSampleDrefExplicitLod = Variadic 8
instructionSize OpImageSparseSampleProjImplicitLod = Variadic 5
instructionSize OpImageSparseSampleProjExplicitLod = Variadic 7
instructionSize OpImageSparseSampleProjDrefImplicitLod = Variadic 6
instructionSize OpImageSparseSampleProjDrefExplicitLod = Variadic 8
instructionSize OpImageSparseFetch = Variadic 5
instructionSize OpImageSparseGather = Variadic 6
instructionSize OpImageSparseDrefGather = Variadic 6
instructionSize OpImageSparseTexelsResident = Fixed 4
instructionSize OpImageSparseRead = Variadic 5
instructionSize OpConvertFToU = Fixed 4
instructionSize OpConvertFToS = Fixed 4
instructionSize OpConvertSToF = Fixed 4
instructionSize OpConvertUToF = Fixed 4
instructionSize OpUConvert = Fixed 4
instructionSize OpSConvert = Fixed 4
instructionSize OpFConvert = Fixed 4
instructionSize OpQuantizeToF16 = Fixed 4
instructionSize OpConvertPtrToU = Fixed 4
instructionSize OpSatConvertSToU = Fixed 4
instructionSize OpSatConvertUToS = Fixed 4
instructionSize OpConvertUToPtr = Fixed 4
instructionSize OpPtrCastToGeneric = Fixed 4
instructionSize OpGenericCastToPtr = Fixed 4
instructionSize OpGenericCastToPtrExplicit = Fixed 5
instructionSize OpBitcast = Fixed 4
instructionSize OpVectorExtractDynamic = Fixed 5
instructionSize OpVectorInsertDynamic = Fixed 6
instructionSize OpVectorShuffle = Variadic 5
instructionSize OpCompositeConstruct = Variadic 3
instructionSize OpCompositeExtract = Variadic 4
instructionSize OpCompositeInsert = Variadic 5
instructionSize OpCopyObject = Fixed 4
instructionSize OpTranspose = Fixed 4
instructionSize OpSNegate = Fixed 4
instructionSize OpFNegate = Fixed 4
instructionSize OpIAdd = Fixed 5
instructionSize OpFAdd = Fixed 5
instructionSize OpISub = Fixed 5
instructionSize OpFSub = Fixed 5
instructionSize OpIMul = Fixed 5
instructionSize OpFMul = Fixed 5
instructionSize OpUDiv = Fixed 5
instructionSize OpSDiv = Fixed 5
instructionSize OpFDiv = Fixed 5
instructionSize OpUMod = Fixed 5
instructionSize OpSRem = Fixed 5
instructionSize OpSMod = Fixed 5
instructionSize OpFRem = Fixed 5
instructionSize OpFMod = Fixed 5
instructionSize OpVectorTimesScalar = Fixed 5
instructionSize OpMatrixTimesScalar = Fixed 5
instructionSize OpVectorTimesMatrix = Fixed 5
instructionSize OpMatrixTimesVector = Fixed 5
instructionSize OpMatrixTimesMatrix = Fixed 5
instructionSize OpOuterProduct = Fixed 5
instructionSize OpDot = Fixed 5
instructionSize OpIAddCarry = Fixed 5
instructionSize OpISubBorrow = Fixed 5
instructionSize OpUMulExtended = Fixed 5
instructionSize OpSMulExtended = Fixed 5
instructionSize OpShiftRightLogical = Fixed 5
instructionSize OpShiftRightArithmetic = Fixed 5
instructionSize OpShiftLeftLogical = Fixed 5
instructionSize OpBitwiseOr = Fixed 5
instructionSize OpBitwiseXor = Fixed 5
instructionSize OpBitwiseAnd = Fixed 5
instructionSize OpNot = Fixed 4
instructionSize OpBitFieldInsert = Fixed 7
instructionSize OpBitFieldSExtract = Fixed 6
instructionSize OpBitFieldUExtract = Fixed 6
instructionSize OpBitReverse = Fixed 4
instructionSize OpBitCount = Fixed 4
instructionSize OpAny = Fixed 4
instructionSize OpAll = Fixed 4
instructionSize OpIsNan = Fixed 4
instructionSize OpIsInf = Fixed 4
instructionSize OpIsFinite = Fixed 4
instructionSize OpIsNormal = Fixed 4
instructionSize OpSignBitSet = Fixed 4
instructionSize OpLessOrGreater = Fixed 5
instructionSize OpOrdered = Fixed 5
instructionSize OpUnordered = Fixed 5
instructionSize OpLogicalEqual = Fixed 5
instructionSize OpLogicalNotEqual = Fixed 5
instructionSize OpLogicalOr = Fixed 5
instructionSize OpLogicalAnd = Fixed 5
instructionSize OpLogicalNot = Fixed 4
instructionSize OpSelect = Fixed 6
instructionSize OpIEqual = Fixed 5
instructionSize OpINotEqual = Fixed 5
instructionSize OpUGreaterThan = Fixed 5
instructionSize OpSGreaterThan = Fixed 5
instructionSize OpUGreaterThanEqual = Fixed 5
instructionSize OpSGreaterThanEqual = Fixed 5
instructionSize OpULessThan = Fixed 5
instructionSize OpSLessThan = Fixed 5
instructionSize OpULessThanEqual = Fixed 5
instructionSize OpSLessThanEqual = Fixed 5
instructionSize OpFOrdEqual = Fixed 5
instructionSize OpFUnordEqual = Fixed 5
instructionSize OpFOrdNotEqual = Fixed 5
instructionSize OpFUnordNotEqual = Fixed 5
instructionSize OpFOrdLessThan = Fixed 5
instructionSize OpFUnordLessThan = Fixed 5
instructionSize OpFOrdGreaterThan = Fixed 5
instructionSize OpFUnordGreaterThan = Fixed 5
instructionSize OpFOrdLessThanEqual = Fixed 5
instructionSize OpFUnordLessThanEqual = Fixed 5
instructionSize OpFOrdGreaterThanEqual = Fixed 5
instructionSize OpFUnordGreaterThanEqual = Fixed 5
instructionSize OpDPdx = Fixed 4
instructionSize OpDPdy = Fixed 4
instructionSize OpFwidth = Fixed 4
instructionSize OpDPdxFine = Fixed 4
instructionSize OpDPdyFine = Fixed 4
instructionSize OpFwidthFine = Fixed 4
instructionSize OpDPdxCoarse = Fixed 4
instructionSize OpDPdyCoarse = Fixed 4
instructionSize OpFwidthCoarse = Fixed 4
instructionSize OpPhi = Variadic 3
instructionSize OpLoopMerge = Variadic 4
instructionSize OpSelectionMerge = Fixed 3
instructionSize OpLabel = Fixed 2
instructionSize OpBranch = Fixed 2
instructionSize OpBranchConditional = Variadic 4
instructionSize OpSwitch = Fixed 3
instructionSize OpKill = Fixed 1
instructionSize OpReturn = Fixed 1
instructionSize OpReturnValue = Fixed 2
instructionSize OpUnreachable = Fixed 1
instructionSize OpLifetimeStart = Fixed 3
instructionSize OpLifetimeStop = Fixed 3
instructionSize OpAtomicLoad = Fixed 6
instructionSize OpAtomicStore = Fixed 5
instructionSize OpAtomicExchange = Fixed 7
instructionSize OpAtomicCompareExchange = Fixed 9
instructionSize OpAtomicCompareExchangeWeak = Fixed 9
instructionSize OpAtomicIIncrement = Fixed 6
instructionSize OpAtomicIDecrement = Fixed 6
instructionSize OpAtomicIAdd = Fixed 7
instructionSize OpAtomicISub = Fixed 7
instructionSize OpAtomicSMin = Fixed 7
instructionSize OpAtomicUMin = Fixed 7
instructionSize OpAtomicSMax = Fixed 7
instructionSize OpAtomicUMax = Fixed 7
instructionSize OpAtomicAnd = Fixed 7
instructionSize OpAtomicOr = Fixed 7
instructionSize OpAtomicXor = Fixed 7
instructionSize OpAtomicFlagTestAndSet = Fixed 6
instructionSize OpAtomicFlagClear = Fixed 4
instructionSize OpEmitVertex = Fixed 1
instructionSize OpEndPrimitive = Fixed 1
instructionSize OpEmitStreamVertex = Fixed 2
instructionSize OpEndStreamPrimitive = Fixed 2
instructionSize OpControlBarrier = Fixed 4
instructionSize OpMemoryBarrier = Fixed 3
instructionSize OpNamedBarrierInitialize = Fixed 4
instructionSize OpMemoryNamedBarrier = Fixed 4
instructionSize OpGroupAsyncCopy = Fixed 9
instructionSize OpGroupWaitEvents = Fixed 4
instructionSize OpGroupAll = Fixed 5
instructionSize OpGroupAny = Fixed 5
instructionSize OpGroupBroadcast = Fixed 6
instructionSize OpGroupIAdd = Fixed 6
instructionSize OpGroupFAdd = Fixed 6
instructionSize OpGroupFMin = Fixed 6
instructionSize OpGroupUMin = Fixed 6
instructionSize OpGroupSMin = Fixed 6
instructionSize OpGroupFMax = Fixed 6
instructionSize OpGroupUMax = Fixed 6
instructionSize OpGroupSMax = Fixed 6
instructionSize OpSubgroupBallotKHR = Fixed 4
instructionSize OpSubgroupFirstInvocationKHR = Fixed 4
instructionSize OpEnqueueMarker = Fixed 7
instructionSize OpEnqueueKernel = Variadic 13
instructionSize OpGetKernelNDrangeSubGroupCount = Fixed 8
instructionSize OpGetKernelNDrangeMaxSubGroupSize = Fixed 8
instructionSize OpGetKernelWorkGroupSize = Fixed 7
instructionSize OpGetKernelPreferredWorkGroupSizeMultiple = Fixed 7
instructionSize OpRetainEvent = Fixed 2
instructionSize OpReleaseEvent = Fixed 2
instructionSize OpCreateUserEvent = Fixed 3
instructionSize OpIsValidEvent = Fixed 4
instructionSize OpSetUserEventStatus = Fixed 3
instructionSize OpCaptureEventProfilingInfo = Fixed 4
instructionSize OpGetDefaultQueue = Fixed 3
instructionSize OpBuildNDRange = Fixed 6
instructionSize OpGetKernelLocalSizeForSubgroupCount = Fixed 8
instructionSize OpGetKernelMaxNumSubgroups = Fixed 7
instructionSize OpReadPipe = Fixed 7
instructionSize OpWritePipe = Fixed 7
instructionSize OpReservedReadPipe = Fixed 9
instructionSize OpReservedWritePipe = Fixed 9
instructionSize OpReserveReadPipePackets = Fixed 7
instructionSize OpReserveWritePipePackets = Fixed 7
instructionSize OpCommitReadPipe = Fixed 5
instructionSize OpCommitWritePipe = Fixed 5
instructionSize OpIsValidReserveId = Fixed 4
instructionSize OpGetNumPipePackets = Fixed 6
instructionSize OpGetMaxPipePackets = Fixed 6
instructionSize OpGroupReserveReadPipePackets = Fixed 8
instructionSize OpGroupReserveWritePipePackets = Fixed 8
instructionSize OpGroupCommitReadPipe = Fixed 6
instructionSize OpGroupCommitWritePipe = Fixed 6
instructionSize OpConstantPipeStorage = Fixed 6
instructionSize OpCreatePipeFromPipeStorage = Fixed 4

instance SpirEnum OpCode Word16 where
  toWord OpNop = 0
  toWord OpUndef = 1
  toWord OpSizeOf = 321
  toWord OpSourceContinued = 2
  toWord OpSource = 3
  toWord OpSourceExtension = 4
  toWord OpName = 5
  toWord OpMemberName = 6
  toWord OpString = 7
  toWord OpLine = 8
  toWord OpNoLine = 317
  toWord OpModuleProcessed = 330
  toWord OpDecorate = 71
  toWord OpMemberDecorate = 72
  toWord OpDecorationGroup = 73
  toWord OpGroupDecorate = 74
  toWord OpGroupMemberDecorate = 75
  toWord OpExtension = 10
  toWord OpExtInstImport = 11
  toWord OpExtInst = 12
  toWord OpMemoryModel = 14
  toWord OpEntryPoint = 15
  toWord OpExecutionMode = 16
  toWord OpCapability = 17
  toWord OpTypeVoid = 19
  toWord OpTypeBool = 20
  toWord OpTypeInt = 21
  toWord OpTypeFloat = 22
  toWord OpTypeVector = 23
  toWord OpTypeMatrix = 24
  toWord OpTypeImage = 25
  toWord OpTypeSampler = 26
  toWord OpTypeSampledImage = 27
  toWord OpTypeArray = 28
  toWord OpTypeRuntimeArray = 29
  toWord OpTypeStruct = 30
  toWord OpTypeOpaque = 31
  toWord OpTypePointer = 32
  toWord OpTypeFunction = 33
  toWord OpTypeEvent = 34
  toWord OpTypeDeviceEvent = 35
  toWord OpTypeReserveId = 36
  toWord OpTypeQueue = 37
  toWord OpTypePipe = 38
  toWord OpTypeForwardPointer = 39
  toWord OpTypePipeStorage = 322
  toWord OpTypeNamedBarrier = 327
  toWord OpConstantTrue = 41
  toWord OpConstantFalse = 42
  toWord OpConstant = 43
  toWord OpConstantComposite = 44
  toWord OpConstantSampler = 45
  toWord OpConstantNull = 46
  toWord OpSpecConstantTrue = 48
  toWord OpSpecConstantFalse = 49
  toWord OpSpecConstant = 50
  toWord OpSpecConstantComposite = 51
  toWord OpSpecConstantOp = 52
  toWord OpVariable = 59
  toWord OpImageTexelPointer = 60
  toWord OpLoad = 61
  toWord OpStore = 62
  toWord OpCopyMemory = 63
  toWord OpCopyMemorySized = 64
  toWord OpAccessChain = 65
  toWord OpInBoundsAccessChain = 66
  toWord OpPtrAccessChain = 67
  toWord OpArrayLength = 68
  toWord OpGenericPtrMemSemantics = 69
  toWord OpInBoundsPtrAccessChain = 70
  toWord OpFunction = 54
  toWord OpFunctionParameter = 55
  toWord OpFunctionEnd = 56
  toWord OpFunctionCall = 57
  toWord OpSampledImage = 86
  toWord OpImageSampleImplicitLod = 87
  toWord OpImageSampleExplicitLod = 88
  toWord OpImageSampleDrefImplicitLod = 89
  toWord OpImageSampleDrefExplicitLod = 90
  toWord OpImageSampleProjImplicitLod = 91
  toWord OpImageSampleProjExplicitLod = 92
  toWord OpImageSampleProjDrefImplicitLod = 93
  toWord OpImageSampleProjDrefExplicitLod = 94
  toWord OpImageFetch = 95
  toWord OpImageGather = 96
  toWord OpImageDrefGather = 97
  toWord OpImageRead = 98
  toWord OpImageWrite = 99
  toWord OpImage = 100
  toWord OpImageQueryFormat = 101
  toWord OpImageQueryOrder = 102
  toWord OpImageQuerySizeLod = 103
  toWord OpImageQuerySize = 104
  toWord OpImageQueryLod = 105
  toWord OpImageQueryLevels = 106
  toWord OpImageQuerySamples = 107
  toWord OpImageSparseSampleImplicitLod = 305
  toWord OpImageSparseSampleExplicitLod = 306
  toWord OpImageSparseSampleDrefImplicitLod = 307
  toWord OpImageSparseSampleDrefExplicitLod = 308
  toWord OpImageSparseSampleProjImplicitLod = 309
  toWord OpImageSparseSampleProjExplicitLod = 310
  toWord OpImageSparseSampleProjDrefImplicitLod = 311
  toWord OpImageSparseSampleProjDrefExplicitLod = 312
  toWord OpImageSparseFetch = 313
  toWord OpImageSparseGather = 314
  toWord OpImageSparseDrefGather = 315
  toWord OpImageSparseTexelsResident = 316
  toWord OpImageSparseRead = 320
  toWord OpConvertFToU = 109
  toWord OpConvertFToS = 110
  toWord OpConvertSToF = 111
  toWord OpConvertUToF = 112
  toWord OpUConvert = 113
  toWord OpSConvert = 114
  toWord OpFConvert = 115
  toWord OpQuantizeToF16 = 116
  toWord OpConvertPtrToU = 117
  toWord OpSatConvertSToU = 118
  toWord OpSatConvertUToS = 119
  toWord OpConvertUToPtr = 120
  toWord OpPtrCastToGeneric = 121
  toWord OpGenericCastToPtr = 122
  toWord OpGenericCastToPtrExplicit = 123
  toWord OpBitcast = 124
  toWord OpVectorExtractDynamic = 77
  toWord OpVectorInsertDynamic = 78
  toWord OpVectorShuffle = 79
  toWord OpCompositeConstruct = 80
  toWord OpCompositeExtract = 81
  toWord OpCompositeInsert = 82
  toWord OpCopyObject = 83
  toWord OpTranspose = 84
  toWord OpSNegate = 126
  toWord OpFNegate = 127
  toWord OpIAdd = 128
  toWord OpFAdd = 129
  toWord OpISub = 130
  toWord OpFSub = 131
  toWord OpIMul = 132
  toWord OpFMul = 133
  toWord OpUDiv = 134
  toWord OpSDiv = 135
  toWord OpFDiv = 136
  toWord OpUMod = 137
  toWord OpSRem = 138
  toWord OpSMod = 139
  toWord OpFRem = 140
  toWord OpFMod = 141
  toWord OpVectorTimesScalar = 142
  toWord OpMatrixTimesScalar = 143
  toWord OpVectorTimesMatrix = 144
  toWord OpMatrixTimesVector = 145
  toWord OpMatrixTimesMatrix = 146
  toWord OpOuterProduct = 147
  toWord OpDot = 148
  toWord OpIAddCarry = 149
  toWord OpISubBorrow = 150
  toWord OpUMulExtended = 151
  toWord OpSMulExtended = 152
  toWord OpShiftRightLogical = 194
  toWord OpShiftRightArithmetic = 195
  toWord OpShiftLeftLogical = 196
  toWord OpBitwiseOr = 197
  toWord OpBitwiseXor = 198
  toWord OpBitwiseAnd = 199
  toWord OpNot = 200
  toWord OpBitFieldInsert = 201
  toWord OpBitFieldSExtract = 202
  toWord OpBitFieldUExtract = 203
  toWord OpBitReverse = 204
  toWord OpBitCount = 205
  toWord OpAny = 154
  toWord OpAll = 155
  toWord OpIsNan = 156
  toWord OpIsInf = 157
  toWord OpIsFinite = 158
  toWord OpIsNormal = 159
  toWord OpSignBitSet = 160
  toWord OpLessOrGreater = 161
  toWord OpOrdered = 162
  toWord OpUnordered = 163
  toWord OpLogicalEqual = 164
  toWord OpLogicalNotEqual = 165
  toWord OpLogicalOr = 166
  toWord OpLogicalAnd = 167
  toWord OpLogicalNot = 168
  toWord OpSelect = 169
  toWord OpIEqual = 170
  toWord OpINotEqual = 171
  toWord OpUGreaterThan = 172
  toWord OpSGreaterThan = 173
  toWord OpUGreaterThanEqual = 174
  toWord OpSGreaterThanEqual = 175
  toWord OpULessThan = 176
  toWord OpSLessThan = 177
  toWord OpULessThanEqual = 178
  toWord OpSLessThanEqual = 179
  toWord OpFOrdEqual = 180
  toWord OpFUnordEqual = 181
  toWord OpFOrdNotEqual = 182
  toWord OpFUnordNotEqual = 183
  toWord OpFOrdLessThan = 184
  toWord OpFUnordLessThan = 185
  toWord OpFOrdGreaterThan = 186
  toWord OpFUnordGreaterThan = 187
  toWord OpFOrdLessThanEqual = 188
  toWord OpFUnordLessThanEqual = 189
  toWord OpFOrdGreaterThanEqual = 190
  toWord OpFUnordGreaterThanEqual = 191
  toWord OpDPdx = 207
  toWord OpDPdy = 208
  toWord OpFwidth = 209
  toWord OpDPdxFine = 210
  toWord OpDPdyFine = 211
  toWord OpFwidthFine = 212
  toWord OpDPdxCoarse = 213
  toWord OpDPdyCoarse = 214
  toWord OpFwidthCoarse = 215
  toWord OpPhi = 245
  toWord OpLoopMerge = 246
  toWord OpSelectionMerge = 247
  toWord OpLabel = 248
  toWord OpBranch = 249
  toWord OpBranchConditional = 250
  toWord OpSwitch = 251
  toWord OpKill = 252
  toWord OpReturn = 253
  toWord OpReturnValue = 254
  toWord OpUnreachable = 255
  toWord OpLifetimeStart = 256
  toWord OpLifetimeStop = 257
  toWord OpAtomicLoad = 227
  toWord OpAtomicStore = 228
  toWord OpAtomicExchange = 229
  toWord OpAtomicCompareExchange = 230
  toWord OpAtomicCompareExchangeWeak = 231
  toWord OpAtomicIIncrement = 232
  toWord OpAtomicIDecrement = 233
  toWord OpAtomicIAdd = 234
  toWord OpAtomicISub = 235
  toWord OpAtomicSMin = 236
  toWord OpAtomicUMin = 237
  toWord OpAtomicSMax = 238
  toWord OpAtomicUMax = 239
  toWord OpAtomicAnd = 240
  toWord OpAtomicOr = 241
  toWord OpAtomicXor = 242
  toWord OpAtomicFlagTestAndSet = 318
  toWord OpAtomicFlagClear = 319
  toWord OpEmitVertex = 218
  toWord OpEndPrimitive = 219
  toWord OpEmitStreamVertex = 220
  toWord OpEndStreamPrimitive = 221
  toWord OpControlBarrier = 224
  toWord OpMemoryBarrier = 225
  toWord OpNamedBarrierInitialize = 328
  toWord OpMemoryNamedBarrier = 329
  toWord OpGroupAsyncCopy = 259
  toWord OpGroupWaitEvents = 260
  toWord OpGroupAll = 261
  toWord OpGroupAny = 262
  toWord OpGroupBroadcast = 263
  toWord OpGroupIAdd = 264
  toWord OpGroupFAdd = 265
  toWord OpGroupFMin = 266
  toWord OpGroupUMin = 267
  toWord OpGroupSMin = 268
  toWord OpGroupFMax = 269
  toWord OpGroupUMax = 270
  toWord OpGroupSMax = 271
  toWord OpSubgroupBallotKHR = 4421
  toWord OpSubgroupFirstInvocationKHR = 4422
  toWord OpEnqueueMarker = 291
  toWord OpEnqueueKernel = 292
  toWord OpGetKernelNDrangeSubGroupCount = 293
  toWord OpGetKernelNDrangeMaxSubGroupSize = 294
  toWord OpGetKernelWorkGroupSize = 295
  toWord OpGetKernelPreferredWorkGroupSizeMultiple = 296
  toWord OpRetainEvent = 297
  toWord OpReleaseEvent = 298
  toWord OpCreateUserEvent = 299
  toWord OpIsValidEvent = 300
  toWord OpSetUserEventStatus = 301
  toWord OpCaptureEventProfilingInfo = 302
  toWord OpGetDefaultQueue = 303
  toWord OpBuildNDRange = 304
  toWord OpGetKernelLocalSizeForSubgroupCount = 325
  toWord OpGetKernelMaxNumSubgroups = 326
  toWord OpReadPipe = 274
  toWord OpWritePipe = 275
  toWord OpReservedReadPipe = 276
  toWord OpReservedWritePipe = 277
  toWord OpReserveReadPipePackets = 278
  toWord OpReserveWritePipePackets = 279
  toWord OpCommitReadPipe = 280
  toWord OpCommitWritePipe = 281
  toWord OpIsValidReserveId = 282
  toWord OpGetNumPipePackets = 283
  toWord OpGetMaxPipePackets = 284
  toWord OpGroupReserveReadPipePackets = 285
  toWord OpGroupReserveWritePipePackets = 286
  toWord OpGroupCommitReadPipe = 287
  toWord OpGroupCommitWritePipe = 288
  toWord OpConstantPipeStorage = 323
  toWord OpCreatePipeFromPipeStorage = 324

  fromWord 0 = Just OpNop
  fromWord 1 = Just OpUndef
  fromWord 321 = Just OpSizeOf
  fromWord 2 = Just OpSourceContinued
  fromWord 3 = Just OpSource
  fromWord 4 = Just OpSourceExtension
  fromWord 5 = Just OpName
  fromWord 6 = Just OpMemberName
  fromWord 7 = Just OpString
  fromWord 8 = Just OpLine
  fromWord 317 = Just OpNoLine
  fromWord 330 = Just OpModuleProcessed
  fromWord 71 = Just OpDecorate
  fromWord 72 = Just OpMemberDecorate
  fromWord 73 = Just OpDecorationGroup
  fromWord 74 = Just OpGroupDecorate
  fromWord 75 = Just OpGroupMemberDecorate
  fromWord 10 = Just OpExtension
  fromWord 11 = Just OpExtInstImport
  fromWord 12 = Just OpExtInst
  fromWord 14 = Just OpMemoryModel
  fromWord 15 = Just OpEntryPoint
  fromWord 16 = Just OpExecutionMode
  fromWord 17 = Just OpCapability
  fromWord 19 = Just OpTypeVoid
  fromWord 20 = Just OpTypeBool
  fromWord 21 = Just OpTypeInt
  fromWord 22 = Just OpTypeFloat
  fromWord 23 = Just OpTypeVector
  fromWord 24 = Just OpTypeMatrix
  fromWord 25 = Just OpTypeImage
  fromWord 26 = Just OpTypeSampler
  fromWord 27 = Just OpTypeSampledImage
  fromWord 28 = Just OpTypeArray
  fromWord 29 = Just OpTypeRuntimeArray
  fromWord 30 = Just OpTypeStruct
  fromWord 31 = Just OpTypeOpaque
  fromWord 32 = Just OpTypePointer
  fromWord 33 = Just OpTypeFunction
  fromWord 34 = Just OpTypeEvent
  fromWord 35 = Just OpTypeDeviceEvent
  fromWord 36 = Just OpTypeReserveId
  fromWord 37 = Just OpTypeQueue
  fromWord 38 = Just OpTypePipe
  fromWord 39 = Just OpTypeForwardPointer
  fromWord 322 = Just OpTypePipeStorage
  fromWord 327 = Just OpTypeNamedBarrier
  fromWord 41 = Just OpConstantTrue
  fromWord 42 = Just OpConstantFalse
  fromWord 43 = Just OpConstant
  fromWord 44 = Just OpConstantComposite
  fromWord 45 = Just OpConstantSampler
  fromWord 46 = Just OpConstantNull
  fromWord 48 = Just OpSpecConstantTrue
  fromWord 49 = Just OpSpecConstantFalse
  fromWord 50 = Just OpSpecConstant
  fromWord 51 = Just OpSpecConstantComposite
  fromWord 52 = Just OpSpecConstantOp
  fromWord 59 = Just OpVariable
  fromWord 60 = Just OpImageTexelPointer
  fromWord 61 = Just OpLoad
  fromWord 62 = Just OpStore
  fromWord 63 = Just OpCopyMemory
  fromWord 64 = Just OpCopyMemorySized
  fromWord 65 = Just OpAccessChain
  fromWord 66 = Just OpInBoundsAccessChain
  fromWord 67 = Just OpPtrAccessChain
  fromWord 68 = Just OpArrayLength
  fromWord 69 = Just OpGenericPtrMemSemantics
  fromWord 70 = Just OpInBoundsPtrAccessChain
  fromWord 54 = Just OpFunction
  fromWord 55 = Just OpFunctionParameter
  fromWord 56 = Just OpFunctionEnd
  fromWord 57 = Just OpFunctionCall
  fromWord 86 = Just OpSampledImage
  fromWord 87 = Just OpImageSampleImplicitLod
  fromWord 88 = Just OpImageSampleExplicitLod
  fromWord 89 = Just OpImageSampleDrefImplicitLod
  fromWord 90 = Just OpImageSampleDrefExplicitLod
  fromWord 91 = Just OpImageSampleProjImplicitLod
  fromWord 92 = Just OpImageSampleProjExplicitLod
  fromWord 93 = Just OpImageSampleProjDrefImplicitLod
  fromWord 94 = Just OpImageSampleProjDrefExplicitLod
  fromWord 95 = Just OpImageFetch
  fromWord 96 = Just OpImageGather
  fromWord 97 = Just OpImageDrefGather
  fromWord 98 = Just OpImageRead
  fromWord 99 = Just OpImageWrite
  fromWord 100 = Just OpImage
  fromWord 101 = Just OpImageQueryFormat
  fromWord 102 = Just OpImageQueryOrder
  fromWord 103 = Just OpImageQuerySizeLod
  fromWord 104 = Just OpImageQuerySize
  fromWord 105 = Just OpImageQueryLod
  fromWord 106 = Just OpImageQueryLevels
  fromWord 107 = Just OpImageQuerySamples
  fromWord 305 = Just OpImageSparseSampleImplicitLod
  fromWord 306 = Just OpImageSparseSampleExplicitLod
  fromWord 307 = Just OpImageSparseSampleDrefImplicitLod
  fromWord 308 = Just OpImageSparseSampleDrefExplicitLod
  fromWord 309 = Just OpImageSparseSampleProjImplicitLod
  fromWord 310 = Just OpImageSparseSampleProjExplicitLod
  fromWord 311 = Just OpImageSparseSampleProjDrefImplicitLod
  fromWord 312 = Just OpImageSparseSampleProjDrefExplicitLod
  fromWord 313 = Just OpImageSparseFetch
  fromWord 314 = Just OpImageSparseGather
  fromWord 315 = Just OpImageSparseDrefGather
  fromWord 316 = Just OpImageSparseTexelsResident
  fromWord 320 = Just OpImageSparseRead
  fromWord 109 = Just OpConvertFToU
  fromWord 110 = Just OpConvertFToS
  fromWord 111 = Just OpConvertSToF
  fromWord 112 = Just OpConvertUToF
  fromWord 113 = Just OpUConvert
  fromWord 114 = Just OpSConvert
  fromWord 115 = Just OpFConvert
  fromWord 116 = Just OpQuantizeToF16
  fromWord 117 = Just OpConvertPtrToU
  fromWord 118 = Just OpSatConvertSToU
  fromWord 119 = Just OpSatConvertUToS
  fromWord 120 = Just OpConvertUToPtr
  fromWord 121 = Just OpPtrCastToGeneric
  fromWord 122 = Just OpGenericCastToPtr
  fromWord 123 = Just OpGenericCastToPtrExplicit
  fromWord 124 = Just OpBitcast
  fromWord 77 = Just OpVectorExtractDynamic
  fromWord 78 = Just OpVectorInsertDynamic
  fromWord 79 = Just OpVectorShuffle
  fromWord 80 = Just OpCompositeConstruct
  fromWord 81 = Just OpCompositeExtract
  fromWord 82 = Just OpCompositeInsert
  fromWord 83 = Just OpCopyObject
  fromWord 84 = Just OpTranspose
  fromWord 126 = Just OpSNegate
  fromWord 127 = Just OpFNegate
  fromWord 128 = Just OpIAdd
  fromWord 129 = Just OpFAdd
  fromWord 130 = Just OpISub
  fromWord 131 = Just OpFSub
  fromWord 132 = Just OpIMul
  fromWord 133 = Just OpFMul
  fromWord 134 = Just OpUDiv
  fromWord 135 = Just OpSDiv
  fromWord 136 = Just OpFDiv
  fromWord 137 = Just OpUMod
  fromWord 138 = Just OpSRem
  fromWord 139 = Just OpSMod
  fromWord 140 = Just OpFRem
  fromWord 141 = Just OpFMod
  fromWord 142 = Just OpVectorTimesScalar
  fromWord 143 = Just OpMatrixTimesScalar
  fromWord 144 = Just OpVectorTimesMatrix
  fromWord 145 = Just OpMatrixTimesVector
  fromWord 146 = Just OpMatrixTimesMatrix
  fromWord 147 = Just OpOuterProduct
  fromWord 148 = Just OpDot
  fromWord 149 = Just OpIAddCarry
  fromWord 150 = Just OpISubBorrow
  fromWord 151 = Just OpUMulExtended
  fromWord 152 = Just OpSMulExtended
  fromWord 194 = Just OpShiftRightLogical
  fromWord 195 = Just OpShiftRightArithmetic
  fromWord 196 = Just OpShiftLeftLogical
  fromWord 197 = Just OpBitwiseOr
  fromWord 198 = Just OpBitwiseXor
  fromWord 199 = Just OpBitwiseAnd
  fromWord 200 = Just OpNot
  fromWord 201 = Just OpBitFieldInsert
  fromWord 202 = Just OpBitFieldSExtract
  fromWord 203 = Just OpBitFieldUExtract
  fromWord 204 = Just OpBitReverse
  fromWord 205 = Just OpBitCount
  fromWord 154 = Just OpAny
  fromWord 155 = Just OpAll
  fromWord 156 = Just OpIsNan
  fromWord 157 = Just OpIsInf
  fromWord 158 = Just OpIsFinite
  fromWord 159 = Just OpIsNormal
  fromWord 160 = Just OpSignBitSet
  fromWord 161 = Just OpLessOrGreater
  fromWord 162 = Just OpOrdered
  fromWord 163 = Just OpUnordered
  fromWord 164 = Just OpLogicalEqual
  fromWord 165 = Just OpLogicalNotEqual
  fromWord 166 = Just OpLogicalOr
  fromWord 167 = Just OpLogicalAnd
  fromWord 168 = Just OpLogicalNot
  fromWord 169 = Just OpSelect
  fromWord 170 = Just OpIEqual
  fromWord 171 = Just OpINotEqual
  fromWord 172 = Just OpUGreaterThan
  fromWord 173 = Just OpSGreaterThan
  fromWord 174 = Just OpUGreaterThanEqual
  fromWord 175 = Just OpSGreaterThanEqual
  fromWord 176 = Just OpULessThan
  fromWord 177 = Just OpSLessThan
  fromWord 178 = Just OpULessThanEqual
  fromWord 179 = Just OpSLessThanEqual
  fromWord 180 = Just OpFOrdEqual
  fromWord 181 = Just OpFUnordEqual
  fromWord 182 = Just OpFOrdNotEqual
  fromWord 183 = Just OpFUnordNotEqual
  fromWord 184 = Just OpFOrdLessThan
  fromWord 185 = Just OpFUnordLessThan
  fromWord 186 = Just OpFOrdGreaterThan
  fromWord 187 = Just OpFUnordGreaterThan
  fromWord 188 = Just OpFOrdLessThanEqual
  fromWord 189 = Just OpFUnordLessThanEqual
  fromWord 190 = Just OpFOrdGreaterThanEqual
  fromWord 191 = Just OpFUnordGreaterThanEqual
  fromWord 207 = Just OpDPdx
  fromWord 208 = Just OpDPdy
  fromWord 209 = Just OpFwidth
  fromWord 210 = Just OpDPdxFine
  fromWord 211 = Just OpDPdyFine
  fromWord 212 = Just OpFwidthFine
  fromWord 213 = Just OpDPdxCoarse
  fromWord 214 = Just OpDPdyCoarse
  fromWord 215 = Just OpFwidthCoarse
  fromWord 245 = Just OpPhi
  fromWord 246 = Just OpLoopMerge
  fromWord 247 = Just OpSelectionMerge
  fromWord 248 = Just OpLabel
  fromWord 249 = Just OpBranch
  fromWord 250 = Just OpBranchConditional
  fromWord 251 = Just OpSwitch
  fromWord 252 = Just OpKill
  fromWord 253 = Just OpReturn
  fromWord 254 = Just OpReturnValue
  fromWord 255 = Just OpUnreachable
  fromWord 256 = Just OpLifetimeStart
  fromWord 257 = Just OpLifetimeStop
  fromWord 227 = Just OpAtomicLoad
  fromWord 228 = Just OpAtomicStore
  fromWord 229 = Just OpAtomicExchange
  fromWord 230 = Just OpAtomicCompareExchange
  fromWord 231 = Just OpAtomicCompareExchangeWeak
  fromWord 232 = Just OpAtomicIIncrement
  fromWord 233 = Just OpAtomicIDecrement
  fromWord 234 = Just OpAtomicIAdd
  fromWord 235 = Just OpAtomicISub
  fromWord 236 = Just OpAtomicSMin
  fromWord 237 = Just OpAtomicUMin
  fromWord 238 = Just OpAtomicSMax
  fromWord 239 = Just OpAtomicUMax
  fromWord 240 = Just OpAtomicAnd
  fromWord 241 = Just OpAtomicOr
  fromWord 242 = Just OpAtomicXor
  fromWord 318 = Just OpAtomicFlagTestAndSet
  fromWord 319 = Just OpAtomicFlagClear
  fromWord 218 = Just OpEmitVertex
  fromWord 219 = Just OpEndPrimitive
  fromWord 220 = Just OpEmitStreamVertex
  fromWord 221 = Just OpEndStreamPrimitive
  fromWord 224 = Just OpControlBarrier
  fromWord 225 = Just OpMemoryBarrier
  fromWord 328 = Just OpNamedBarrierInitialize
  fromWord 329 = Just OpMemoryNamedBarrier
  fromWord 259 = Just OpGroupAsyncCopy
  fromWord 260 = Just OpGroupWaitEvents
  fromWord 261 = Just OpGroupAll
  fromWord 262 = Just OpGroupAny
  fromWord 263 = Just OpGroupBroadcast
  fromWord 264 = Just OpGroupIAdd
  fromWord 265 = Just OpGroupFAdd
  fromWord 266 = Just OpGroupFMin
  fromWord 267 = Just OpGroupUMin
  fromWord 268 = Just OpGroupSMin
  fromWord 269 = Just OpGroupFMax
  fromWord 270 = Just OpGroupUMax
  fromWord 271 = Just OpGroupSMax
  fromWord 4421 = Just OpSubgroupBallotKHR
  fromWord 4422 = Just OpSubgroupFirstInvocationKHR
  fromWord 291 = Just OpEnqueueMarker
  fromWord 292 = Just OpEnqueueKernel
  fromWord 293 = Just OpGetKernelNDrangeSubGroupCount
  fromWord 294 = Just OpGetKernelNDrangeMaxSubGroupSize
  fromWord 295 = Just OpGetKernelWorkGroupSize
  fromWord 296 = Just OpGetKernelPreferredWorkGroupSizeMultiple
  fromWord 297 = Just OpRetainEvent
  fromWord 298 = Just OpReleaseEvent
  fromWord 299 = Just OpCreateUserEvent
  fromWord 300 = Just OpIsValidEvent
  fromWord 301 = Just OpSetUserEventStatus
  fromWord 302 = Just OpCaptureEventProfilingInfo
  fromWord 303 = Just OpGetDefaultQueue
  fromWord 304 = Just OpBuildNDRange
  fromWord 325 = Just OpGetKernelLocalSizeForSubgroupCount
  fromWord 326 = Just OpGetKernelMaxNumSubgroups
  fromWord 274 = Just OpReadPipe
  fromWord 275 = Just OpWritePipe
  fromWord 276 = Just OpReservedReadPipe
  fromWord 277 = Just OpReservedWritePipe
  fromWord 278 = Just OpReserveReadPipePackets
  fromWord 279 = Just OpReserveWritePipePackets
  fromWord 280 = Just OpCommitReadPipe
  fromWord 281 = Just OpCommitWritePipe
  fromWord 282 = Just OpIsValidReserveId
  fromWord 283 = Just OpGetNumPipePackets
  fromWord 284 = Just OpGetMaxPipePackets
  fromWord 285 = Just OpGroupReserveReadPipePackets
  fromWord 286 = Just OpGroupReserveWritePipePackets
  fromWord 287 = Just OpGroupCommitReadPipe
  fromWord 288 = Just OpGroupCommitWritePipe
  fromWord 323 = Just OpConstantPipeStorage
  fromWord 324 = Just OpCreatePipeFromPipeStorage
  fromWord _ = Nothing

  requiredCapabilities OpSizeOf = [Capability.Addresses]
  requiredCapabilities OpTypeMatrix = [Capability.Matrix]
  requiredCapabilities OpTypeRuntimeArray = [Capability.Shader]
  requiredCapabilities OpTypeOpaque = [Capability.Kernel]
  requiredCapabilities OpTypeEvent = [Capability.Kernel]
  requiredCapabilities OpTypeDeviceEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpTypeReserveId = [Capability.Pipes]
  requiredCapabilities OpTypeQueue = [Capability.DeviceEnqueue]
  requiredCapabilities OpTypePipe = [Capability.Pipes]
  requiredCapabilities OpTypeForwardPointer = [Capability.Addresses]
  requiredCapabilities OpTypePipeStorage = [Capability.PipeStorage]
  requiredCapabilities OpTypeNamedBarrier = [Capability.NamedBarrier]
  requiredCapabilities OpConstantSampler = [Capability.LiteralSampler]
  requiredCapabilities OpCopyMemorySized = [Capability.Addresses]
  requiredCapabilities OpPtrAccessChain = [Capability.Addresses]
  requiredCapabilities OpArrayLength = [Capability.Shader]
  requiredCapabilities OpGenericPtrMemSemantics = [Capability.Kernel]
  requiredCapabilities OpInBoundsPtrAccessChain = [Capability.Addresses]
  requiredCapabilities OpImageSampleImplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleDrefImplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleDrefExplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleProjImplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleProjExplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleProjDrefImplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleProjDrefExplicitLod = [Capability.Shader]
  requiredCapabilities OpImageGather = [Capability.Shader]
  requiredCapabilities OpImageDrefGather = [Capability.Shader]
  requiredCapabilities OpImageQueryFormat = [Capability.Kernel]
  requiredCapabilities OpImageQueryOrder = [Capability.Kernel]
  requiredCapabilities OpImageQuerySizeLod = [Capability.KernelImageQuery]
  requiredCapabilities OpImageQuerySize = [Capability.KernelImageQuery]
  requiredCapabilities OpImageQueryLod = [Capability.ImageQuery]
  requiredCapabilities OpImageQueryLevels = [Capability.KernelImageQuery]
  requiredCapabilities OpImageQuerySamples = [Capability.KernelImageQuery]
  requiredCapabilities OpImageSparseSampleImplicitLod = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseSampleExplicitLod = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseSampleDrefImplicitLod = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseSampleDrefExplicitLod = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseSampleProjImplicitLod = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseSampleProjExplicitLod = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseSampleProjDrefImplicitLod = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseSampleProjDrefExplicitLod = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseFetch = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseGather = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseDrefGather = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseTexelsResident = [Capability.SparseResidency]
  requiredCapabilities OpImageSparseRead = [Capability.SparseResidency]
  requiredCapabilities OpQuantizeToF16 = [Capability.Shader]
  requiredCapabilities OpConvertPtrToU = [Capability.Addresses]
  requiredCapabilities OpSatConvertSToU = [Capability.Kernel]
  requiredCapabilities OpSatConvertUToS = [Capability.Kernel]
  requiredCapabilities OpConvertUToPtr = [Capability.Addresses]
  requiredCapabilities OpPtrCastToGeneric = [Capability.Kernel]
  requiredCapabilities OpGenericCastToPtr = [Capability.Kernel]
  requiredCapabilities OpGenericCastToPtrExplicit = [Capability.Kernel]
  requiredCapabilities OpTranspose = [Capability.Matrix]
  requiredCapabilities OpMatrixTimesScalar = [Capability.Matrix]
  requiredCapabilities OpVectorTimesMatrix = [Capability.Matrix]
  requiredCapabilities OpMatrixTimesVector = [Capability.Matrix]
  requiredCapabilities OpMatrixTimesMatrix = [Capability.Matrix]
  requiredCapabilities OpOuterProduct = [Capability.Matrix]
  requiredCapabilities OpBitFieldInsert = [Capability.Shader]
  requiredCapabilities OpBitFieldSExtract = [Capability.Shader]
  requiredCapabilities OpBitFieldUExtract = [Capability.Shader]
  requiredCapabilities OpBitReverse = [Capability.Shader]
  requiredCapabilities OpIsFinite = [Capability.Kernel]
  requiredCapabilities OpIsNormal = [Capability.Kernel]
  requiredCapabilities OpSignBitSet = [Capability.Kernel]
  requiredCapabilities OpLessOrGreater = [Capability.Kernel]
  requiredCapabilities OpOrdered = [Capability.Kernel]
  requiredCapabilities OpUnordered = [Capability.Kernel]
  requiredCapabilities OpDPdx = [Capability.Shader]
  requiredCapabilities OpDPdy = [Capability.Shader]
  requiredCapabilities OpFwidth = [Capability.Shader]
  requiredCapabilities OpDPdxFine = [Capability.DerivativeControl]
  requiredCapabilities OpDPdyFine = [Capability.DerivativeControl]
  requiredCapabilities OpFwidthFine = [Capability.DerivativeControl]
  requiredCapabilities OpDPdxCoarse = [Capability.DerivativeControl]
  requiredCapabilities OpDPdyCoarse = [Capability.DerivativeControl]
  requiredCapabilities OpFwidthCoarse = [Capability.DerivativeControl]
  requiredCapabilities OpKill = [Capability.Shader]
  requiredCapabilities OpLifetimeStart = [Capability.Kernel]
  requiredCapabilities OpLifetimeStop = [Capability.Kernel]
  requiredCapabilities OpAtomicCompareExchangeWeak = [Capability.Kernel]
  requiredCapabilities OpAtomicFlagTestAndSet = [Capability.Kernel]
  requiredCapabilities OpAtomicFlagClear = [Capability.Kernel]
  requiredCapabilities OpEmitVertex = [Capability.Geometry]
  requiredCapabilities OpEndPrimitive = [Capability.Geometry]
  requiredCapabilities OpEmitStreamVertex = [Capability.GeometryStreams]
  requiredCapabilities OpEndStreamPrimitive = [Capability.GeometryStreams]
  requiredCapabilities OpNamedBarrierInitialize = [Capability.NamedBarrier]
  requiredCapabilities OpMemoryNamedBarrier = [Capability.NamedBarrier]
  requiredCapabilities OpGroupAsyncCopy = [Capability.Kernel]
  requiredCapabilities OpGroupWaitEvents = [Capability.Kernel]
  requiredCapabilities OpGroupAll = [Capability.Groups]
  requiredCapabilities OpGroupAny = [Capability.Groups]
  requiredCapabilities OpGroupBroadcast = [Capability.Groups]
  requiredCapabilities OpGroupIAdd = [Capability.Groups]
  requiredCapabilities OpGroupFAdd = [Capability.Groups]
  requiredCapabilities OpGroupFMin = [Capability.Groups]
  requiredCapabilities OpGroupUMin = [Capability.Groups]
  requiredCapabilities OpGroupSMin = [Capability.Groups]
  requiredCapabilities OpGroupFMax = [Capability.Groups]
  requiredCapabilities OpGroupUMax = [Capability.Groups]
  requiredCapabilities OpGroupSMax = [Capability.Groups]
  requiredCapabilities OpSubgroupBallotKHR = [Capability.SubgroupBallotKHR]
  requiredCapabilities OpSubgroupFirstInvocationKHR = [Capability.SubgroupBallotKHR]
  requiredCapabilities OpEnqueueMarker = [Capability.DeviceEnqueue]
  requiredCapabilities OpEnqueueKernel = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelNDrangeSubGroupCount = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelNDrangeMaxSubGroupSize = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelWorkGroupSize = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelPreferredWorkGroupSizeMultiple = [Capability.DeviceEnqueue]
  requiredCapabilities OpRetainEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpReleaseEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpCreateUserEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpIsValidEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpSetUserEventStatus = [Capability.DeviceEnqueue]
  requiredCapabilities OpCaptureEventProfilingInfo = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetDefaultQueue = [Capability.DeviceEnqueue]
  requiredCapabilities OpBuildNDRange = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelLocalSizeForSubgroupCount = [Capability.SubgroupDispatch]
  requiredCapabilities OpGetKernelMaxNumSubgroups = [Capability.SubgroupDispatch]
  requiredCapabilities OpReadPipe = [Capability.Pipes]
  requiredCapabilities OpWritePipe = [Capability.Pipes]
  requiredCapabilities OpReservedReadPipe = [Capability.Pipes]
  requiredCapabilities OpReservedWritePipe = [Capability.Pipes]
  requiredCapabilities OpReserveReadPipePackets = [Capability.Pipes]
  requiredCapabilities OpReserveWritePipePackets = [Capability.Pipes]
  requiredCapabilities OpCommitReadPipe = [Capability.Pipes]
  requiredCapabilities OpCommitWritePipe = [Capability.Pipes]
  requiredCapabilities OpIsValidReserveId = [Capability.Pipes]
  requiredCapabilities OpGetNumPipePackets = [Capability.Pipes]
  requiredCapabilities OpGetMaxPipePackets = [Capability.Pipes]
  requiredCapabilities OpGroupReserveReadPipePackets = [Capability.Pipes]
  requiredCapabilities OpGroupReserveWritePipePackets = [Capability.Pipes]
  requiredCapabilities OpGroupCommitReadPipe = [Capability.Pipes]
  requiredCapabilities OpGroupCommitWritePipe = [Capability.Pipes]
  requiredCapabilities OpConstantPipeStorage = [Capability.PipeStorage]
  requiredCapabilities OpCreatePipeFromPipeStorage = [Capability.PipeStorage]
  requiredCapabilities _ = []
