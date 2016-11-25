{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.Capability where

import Data.Word (Word32)
import Language.SpirV.SpirEnum


-- | Capabilities a module can declare it uses. All used capabilities must be declared, either directly or through a dependency: all capabilities that a declared capability depends on are automatically implied.
--
--  The __Depends On__ column lists the dependencies for each capability. These are the ones implicitly declared. It is not necessary (but allowed) to declare a dependency for a declared capability.
--
--  See the <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Capabilities capabilities> section for more detail. Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCapability OpCapability>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_capability_a_capability Source for Capability>
data Capability =
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeMatrix OpTypeMatrix>__.
    Matrix
    -- | Uses __Vertex__, __Fragment__, or __GLCompute__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | Shader
    -- | Uses the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | Geometry
    -- | Uses the __TessellationControl__ or __TessellationEvaluation__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | Tessellation
    -- | Uses physical addressing, non-logical addressing modes.
  | Addresses
    -- | Uses partially linked modules and libraries.
  | Linkage
    -- | Uses the __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | Kernel
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeVector OpTypeVector>__ to declare 8 component or 16 component vectors.
  | Vector16
    -- | Allows a 16-bit __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeFloat OpTypeFloat>__ instruction for the sole purpose of creating an __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypePointer OpTypePointer>__ to a 16-bit float. Pointers to a 16-bit float cannot be dereferenced directly, they must only be dereferenced via an extended instruction. All other uses of 16-bit __OpTypeFloat__ are disallowed.
  | Float16Buffer
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeFloat OpTypeFloat>__ to declare the 16-bit floating-point type.
  | Float16
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeFloat OpTypeFloat>__ to declare the 64-bit floating-point type.
  | Float64
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeInt OpTypeInt>__ to declare 64-bit integer types.
  | Int64
    -- | Uses atomic instructions on 64-bit integer types.
  | Int64Atomics
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ or __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeSampler OpTypeSampler>__ in a __Kernel__.
  | ImageBasic
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>__ with the __ReadWrite__ /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Access_Qualifier access qualifier>/.
  | ImageReadWrite
    -- | Uses non-zero __Lod__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Image_Operands Image Operands>.
  | ImageMipmap
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypePipe OpTypePipe>__, __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeReserveId OpTypeReserveId>__ or /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Pipe pipe>/ instructions.
  | Pipes
    -- | Uses /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Group group>/ instructions.
  | Groups
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeQueue OpTypeQueue>__, __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeDeviceEvent OpTypeDeviceEvent>__, and /<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Device-Side_Enqueue device side enqueue>/ instructions.
  | DeviceEnqueue
    -- | <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler Samplers> are made from literals within the module. See __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpConstantSampler OpConstantSampler>__.
  | LiteralSampler
    -- | Uses the __AtomicCounter__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class>.
  | AtomicStorage
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeInt OpTypeInt>__ to declare 16-bit integer types.
  | Int16
    -- | Tessellation stage exports point size.
  | TessellationPointSize
    -- | Geometry stage exports point size
  | GeometryPointSize
    -- | Uses texture gather with non-constant or independent offsets
  | ImageGatherExtended
    -- | Uses multi-sample images for non-sampled images.
  | StorageImageMultisample
    -- | __Block__-decorated arrays in uniform storage classes use <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#DynamicallyUniform dynamically uniform> indexing.
  | UniformBufferArrayDynamicIndexing
    -- | Arrays of sampled images use <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#DynamicallyUniform dynamically uniform> indexing.
  | SampledImageArrayDynamicIndexing
    -- | __BufferBlock__-decorated arrays in uniform storage classes use <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#DynamicallyUniform dynamically uniform> indexing.
  | StorageBufferArrayDynamicIndexing
    -- | Arrays of non-sampled images are accessed with <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#DynamicallyUniform dynamically uniform> indexing.
  | StorageImageArrayDynamicIndexing
    -- | Uses the __ClipDistance__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#BuiltIn BuiltIn>.
  | ClipDistance
    -- | Uses the __CullDistance__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#BuiltIn BuiltIn>.
  | CullDistance
    -- | Uses the __Cube__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim> with the /Arrayed/ operand in <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>, without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | ImageCubeArray
    -- | Uses per-sample rate shading.
  | SampleRateShading
    -- | Uses the __Rect__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim> without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | ImageRect
    -- | Uses the __Rect__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim> with a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | SampledRect
    -- | Uses the __Generic__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Storage_Class Storage Class>.
  | GenericPointer
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeInt OpTypeInt>__ to declare 8-bit integer types.
  | Int8
    -- | Uses the __SubpassData__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim>.
  | InputAttachment
    -- | Uses __OpImageSparse…__ instructions.
  | SparseResidency
    -- | Uses the __MinLod__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Image_Operands Image Operand>.
  | MinLod
    -- | Uses the __1D__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim> with a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | Sampled1D
    -- | Uses the __1D__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim> without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | Image1D
    -- | Uses the __Cube__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim> with the /Arrayed/ operand in <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage>, with a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | SampledCubeArray
    -- | Uses the __Buffer__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim> without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | SampledBuffer
    -- | Uses the __Buffer__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Dim Dim> without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | ImageBuffer
    -- | An /MS/ operand in <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeImage OpTypeImage> indicates multisampled, used without a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Sampler sampler>.
  | ImageMSArray
    -- | One of a large set of more advanced image formats are used, namely one of those in the <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Image_Format Image Format> table listed as requiring this capability.
  | StorageImageExtendedFormats
    -- | The sizes, number of samples, or lod, etc. are queried.
  | ImageQuery
    -- | Uses fine or coarse-grained derivatives, e.g., __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpDPdxFine OpDPdxFine>__.
  | DerivativeControl
    -- | Uses one of the __InterpolateAtCentroid__, __InterpolateAtSample__, or __InterpolateAtOffset__ GLSL.std.450 extended instructions.
  | InterpolationFunction
    -- | Uses the __Xfb__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Mode Execution Mode>.
  | TransformFeedback
    -- | Uses multiple numbered streams for geometry-stage output.
  | GeometryStreams
    -- | __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageRead OpImageRead>__ can use the __Unknown__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Image_Format Image Format> for
  | StorageImageReadWithoutFormat
    -- | __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpImageWrite OpImageWrite>__ can use the __Unknown__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Image_Format Image Format>.
  | StorageImageWriteWithoutFormat
    -- | Multiple viewports are used.
  | MultiViewport
    -- | Uses subgroup dispatch instructions.
  | SubgroupDispatch
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypeNamedBarrier OpTypeNamedBarrier>__.
  | NamedBarrier
    -- | Uses __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpTypePipeStorage OpTypePipeStorage>__.
  | PipeStorage
  
  | SubgroupBallotKHR
  
  | DrawParameters
  deriving(Read, Show, Eq, Ord)

instance SpirEnum Capability Word32 where
  toWord Matrix = 0
  toWord Shader = 1
  toWord Geometry = 2
  toWord Tessellation = 3
  toWord Addresses = 4
  toWord Linkage = 5
  toWord Kernel = 6
  toWord Vector16 = 7
  toWord Float16Buffer = 8
  toWord Float16 = 9
  toWord Float64 = 10
  toWord Int64 = 11
  toWord Int64Atomics = 12
  toWord ImageBasic = 13
  toWord ImageReadWrite = 14
  toWord ImageMipmap = 15
  toWord Pipes = 17
  toWord Groups = 18
  toWord DeviceEnqueue = 19
  toWord LiteralSampler = 20
  toWord AtomicStorage = 21
  toWord Int16 = 22
  toWord TessellationPointSize = 23
  toWord GeometryPointSize = 24
  toWord ImageGatherExtended = 25
  toWord StorageImageMultisample = 27
  toWord UniformBufferArrayDynamicIndexing = 28
  toWord SampledImageArrayDynamicIndexing = 29
  toWord StorageBufferArrayDynamicIndexing = 30
  toWord StorageImageArrayDynamicIndexing = 31
  toWord ClipDistance = 32
  toWord CullDistance = 33
  toWord ImageCubeArray = 34
  toWord SampleRateShading = 35
  toWord ImageRect = 36
  toWord SampledRect = 37
  toWord GenericPointer = 38
  toWord Int8 = 39
  toWord InputAttachment = 40
  toWord SparseResidency = 41
  toWord MinLod = 42
  toWord Sampled1D = 43
  toWord Image1D = 44
  toWord SampledCubeArray = 45
  toWord SampledBuffer = 46
  toWord ImageBuffer = 47
  toWord ImageMSArray = 48
  toWord StorageImageExtendedFormats = 49
  toWord ImageQuery = 50
  toWord DerivativeControl = 51
  toWord InterpolationFunction = 52
  toWord TransformFeedback = 53
  toWord GeometryStreams = 54
  toWord StorageImageReadWithoutFormat = 55
  toWord StorageImageWriteWithoutFormat = 56
  toWord MultiViewport = 57
  toWord SubgroupDispatch = 58
  toWord NamedBarrier = 59
  toWord PipeStorage = 60
  toWord SubgroupBallotKHR = 4423
  toWord DrawParameters = 4427

  fromWord 0 = Just Matrix
  fromWord 1 = Just Shader
  fromWord 2 = Just Geometry
  fromWord 3 = Just Tessellation
  fromWord 4 = Just Addresses
  fromWord 5 = Just Linkage
  fromWord 6 = Just Kernel
  fromWord 7 = Just Vector16
  fromWord 8 = Just Float16Buffer
  fromWord 9 = Just Float16
  fromWord 10 = Just Float64
  fromWord 11 = Just Int64
  fromWord 12 = Just Int64Atomics
  fromWord 13 = Just ImageBasic
  fromWord 14 = Just ImageReadWrite
  fromWord 15 = Just ImageMipmap
  fromWord 17 = Just Pipes
  fromWord 18 = Just Groups
  fromWord 19 = Just DeviceEnqueue
  fromWord 20 = Just LiteralSampler
  fromWord 21 = Just AtomicStorage
  fromWord 22 = Just Int16
  fromWord 23 = Just TessellationPointSize
  fromWord 24 = Just GeometryPointSize
  fromWord 25 = Just ImageGatherExtended
  fromWord 27 = Just StorageImageMultisample
  fromWord 28 = Just UniformBufferArrayDynamicIndexing
  fromWord 29 = Just SampledImageArrayDynamicIndexing
  fromWord 30 = Just StorageBufferArrayDynamicIndexing
  fromWord 31 = Just StorageImageArrayDynamicIndexing
  fromWord 32 = Just ClipDistance
  fromWord 33 = Just CullDistance
  fromWord 34 = Just ImageCubeArray
  fromWord 35 = Just SampleRateShading
  fromWord 36 = Just ImageRect
  fromWord 37 = Just SampledRect
  fromWord 38 = Just GenericPointer
  fromWord 39 = Just Int8
  fromWord 40 = Just InputAttachment
  fromWord 41 = Just SparseResidency
  fromWord 42 = Just MinLod
  fromWord 43 = Just Sampled1D
  fromWord 44 = Just Image1D
  fromWord 45 = Just SampledCubeArray
  fromWord 46 = Just SampledBuffer
  fromWord 47 = Just ImageBuffer
  fromWord 48 = Just ImageMSArray
  fromWord 49 = Just StorageImageExtendedFormats
  fromWord 50 = Just ImageQuery
  fromWord 51 = Just DerivativeControl
  fromWord 52 = Just InterpolationFunction
  fromWord 53 = Just TransformFeedback
  fromWord 54 = Just GeometryStreams
  fromWord 55 = Just StorageImageReadWithoutFormat
  fromWord 56 = Just StorageImageWriteWithoutFormat
  fromWord 57 = Just MultiViewport
  fromWord 58 = Just SubgroupDispatch
  fromWord 59 = Just NamedBarrier
  fromWord 60 = Just PipeStorage
  fromWord 4423 = Just SubgroupBallotKHR
  fromWord 4427 = Just DrawParameters
  fromWord _ = Nothing

  requiredCapabilities Shader = [Matrix]
  requiredCapabilities Geometry = [Shader]
  requiredCapabilities Tessellation = [Shader]
  requiredCapabilities Vector16 = [Kernel]
  requiredCapabilities Float16Buffer = [Kernel]
  requiredCapabilities Int64Atomics = [Int64]
  requiredCapabilities ImageBasic = [Kernel]
  requiredCapabilities ImageReadWrite = [ImageBasic]
  requiredCapabilities ImageMipmap = [ImageBasic]
  requiredCapabilities Pipes = [Kernel]
  requiredCapabilities DeviceEnqueue = [Kernel]
  requiredCapabilities LiteralSampler = [Kernel]
  requiredCapabilities AtomicStorage = [Shader]
  requiredCapabilities TessellationPointSize = [Tessellation]
  requiredCapabilities GeometryPointSize = [Geometry]
  requiredCapabilities ImageGatherExtended = [Shader]
  requiredCapabilities StorageImageMultisample = [Shader]
  requiredCapabilities UniformBufferArrayDynamicIndexing = [Shader]
  requiredCapabilities SampledImageArrayDynamicIndexing = [Shader]
  requiredCapabilities StorageBufferArrayDynamicIndexing = [Shader]
  requiredCapabilities StorageImageArrayDynamicIndexing = [Shader]
  requiredCapabilities ClipDistance = [Shader]
  requiredCapabilities CullDistance = [Shader]
  requiredCapabilities ImageCubeArray = [SampledCubeArray]
  requiredCapabilities SampleRateShading = [Shader]
  requiredCapabilities ImageRect = [SampledRect]
  requiredCapabilities SampledRect = [Shader]
  requiredCapabilities GenericPointer = [Addresses]
  requiredCapabilities Int8 = [Kernel]
  requiredCapabilities InputAttachment = [Shader]
  requiredCapabilities SparseResidency = [Shader]
  requiredCapabilities MinLod = [Shader]
  requiredCapabilities Image1D = [Sampled1D]
  requiredCapabilities SampledCubeArray = [Shader]
  requiredCapabilities ImageBuffer = [SampledBuffer]
  requiredCapabilities ImageMSArray = [Shader]
  requiredCapabilities StorageImageExtendedFormats = [Shader]
  requiredCapabilities ImageQuery = [Shader]
  requiredCapabilities DerivativeControl = [Shader]
  requiredCapabilities InterpolationFunction = [Shader]
  requiredCapabilities TransformFeedback = [Shader]
  requiredCapabilities GeometryStreams = [Geometry]
  requiredCapabilities StorageImageReadWithoutFormat = [Shader]
  requiredCapabilities StorageImageWriteWithoutFormat = [Shader]
  requiredCapabilities MultiViewport = [Geometry]
  requiredCapabilities SubgroupDispatch = [DeviceEnqueue]
  requiredCapabilities NamedBarrier = [Kernel]
  requiredCapabilities PipeStorage = [Pipes]
  requiredCapabilities _ = []
