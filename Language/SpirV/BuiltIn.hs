{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.BuiltIn where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Used when __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decoration>__ is __BuiltIn__. Apply to either
--
-- -   the result /\<id>/ of the variable declaration of the built-in variable, or
--
-- -   a structure-type member, if the built-in is a member of a structure.
--
-- As stated per entry below, these have additional semantics and constraints described by the client API.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_builtin_a_builtin Source for BuiltIn>
data BuiltIn =
    -- | Output vertex position from a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#VertexProcessor vertex processing> <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
    Position
    -- | Output point size from a <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#VertexProcessor vertex processing> <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | PointSize
    -- | Array of clip distances. See Vulkan or OpenGL API specifications for more detail.
  | ClipDistance
    -- | Array of clip distances. See Vulkan or OpenGL API specifications for more detail.
  | CullDistance
    -- | Input vertex ID to a __Vertex__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | VertexId
    -- | Input instance ID to a __Vertex__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | InstanceId
    -- | Primitive ID in a __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | PrimitiveId
    -- | Invocation ID, input to __Geometry__ and __TessellationControl__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | InvocationId
    -- | Layer output by a __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>, input to a __Fragment__ Execution Model, for multi-layer framebuffer. See Vulkan or OpenGL API specifications for more detail.
  | Layer
    -- | Viewport Index output by a __Geometry__ stage, input to a __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | ViewportIndex
    -- | Output patch outer levels in a __TessellationControl__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | TessLevelOuter
    -- | Output patch inner levels in a __TessellationControl__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | TessLevelInner
    -- | Input vertex position in __TessellationEvaluation__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | TessCoord
    -- | Input patch vertex count in a tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | PatchVertices
    -- | Coordinates /(x, y, z, 1\/w)/ of the current fragment, input to the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | FragCoord
    -- | Coordinates within a /point/, input to the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | PointCoord
    -- | Face direction, input to the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | FrontFacing
    -- | Input sample number to the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | SampleId
    -- | Input sample position to the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | SamplePosition
    -- | Input or output sample mask to the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | SampleMask
    -- | Output fragment depth from the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | FragDepth
    -- | Input whether a helper invocation, to the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. See Vulkan or OpenGL API specifications for more detail.
  | HelperInvocation
    -- | Number of workgroups in __GLCompute__ or __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL, Vulkan, or OpenGL API specifications for more detail.
  | NumWorkgroups
    -- | Work-group size in __GLCompute__ or __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL, Vulkan, or OpenGL API specifications for more detail.
  | WorkgroupSize
    -- | Work-group ID in __GLCompute__ or __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL, Vulkan, or OpenGL API specifications for more detail.
  | WorkgroupId
    -- | Local invocation ID in __GLCompute__ or __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL, Vulkan, or OpenGL API specifications for more detail.
  | LocalInvocationId
    -- | Global invocation ID in __GLCompute__ or __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL, Vulkan, or OpenGL API specifications for more detail.
  | GlobalInvocationId
    -- | Local invocation index in __GLCompute__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See Vulkan or OpenGL API specifications for more detail.
    --
    --  Work-group Linear ID in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | LocalInvocationIndex
    -- | Work dimensions in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | WorkDim
    -- | Global size in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | GlobalSize
    -- | Enqueued work-group size in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | EnqueuedWorkgroupSize
    -- | Global offset in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | GlobalOffset
    -- | Global linear ID in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | GlobalLinearId
    -- | Subgroup size in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | SubgroupSize
    -- | Subgroup maximum size in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | SubgroupMaxSize
    -- | Number of subgroups in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | NumSubgroups
    -- | Number of enqueued subgroups in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | NumEnqueuedSubgroups
    -- | Subgroup ID in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | SubgroupId
    -- | Subgroup local invocation ID in __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>. See OpenCL API specification for more detail.
  | SubgroupLocalInvocationId
    -- | Vertex index. See Vulkan or OpenGL API specifications for more detail.
  | VertexIndex
    -- | Instance index. See Vulkan or OpenGL API specifications for more detail.
  | InstanceIndex
  
  | SubgroupEqMaskKHR
  
  | SubgroupGeMaskKHR
  
  | SubgroupGtMaskKHR
  
  | SubgroupLeMaskKHR
  
  | SubgroupLtMaskKHR
  
  | BaseVertex
  
  | BaseInstance
  
  | DrawIndex
  deriving(Read, Show, Eq, Ord)

instance SpirEnum BuiltIn Word32 where
  toWord Position = 0
  toWord PointSize = 1
  toWord ClipDistance = 3
  toWord CullDistance = 4
  toWord VertexId = 5
  toWord InstanceId = 6
  toWord PrimitiveId = 7
  toWord InvocationId = 8
  toWord Layer = 9
  toWord ViewportIndex = 10
  toWord TessLevelOuter = 11
  toWord TessLevelInner = 12
  toWord TessCoord = 13
  toWord PatchVertices = 14
  toWord FragCoord = 15
  toWord PointCoord = 16
  toWord FrontFacing = 17
  toWord SampleId = 18
  toWord SamplePosition = 19
  toWord SampleMask = 20
  toWord FragDepth = 22
  toWord HelperInvocation = 23
  toWord NumWorkgroups = 24
  toWord WorkgroupSize = 25
  toWord WorkgroupId = 26
  toWord LocalInvocationId = 27
  toWord GlobalInvocationId = 28
  toWord LocalInvocationIndex = 29
  toWord WorkDim = 30
  toWord GlobalSize = 31
  toWord EnqueuedWorkgroupSize = 32
  toWord GlobalOffset = 33
  toWord GlobalLinearId = 34
  toWord SubgroupSize = 36
  toWord SubgroupMaxSize = 37
  toWord NumSubgroups = 38
  toWord NumEnqueuedSubgroups = 39
  toWord SubgroupId = 40
  toWord SubgroupLocalInvocationId = 41
  toWord VertexIndex = 42
  toWord InstanceIndex = 43
  toWord SubgroupEqMaskKHR = 4416
  toWord SubgroupGeMaskKHR = 4417
  toWord SubgroupGtMaskKHR = 4418
  toWord SubgroupLeMaskKHR = 4419
  toWord SubgroupLtMaskKHR = 4420
  toWord BaseVertex = 4424
  toWord BaseInstance = 4425
  toWord DrawIndex = 4426

  fromWord 0 = Just Position
  fromWord 1 = Just PointSize
  fromWord 3 = Just ClipDistance
  fromWord 4 = Just CullDistance
  fromWord 5 = Just VertexId
  fromWord 6 = Just InstanceId
  fromWord 7 = Just PrimitiveId
  fromWord 8 = Just InvocationId
  fromWord 9 = Just Layer
  fromWord 10 = Just ViewportIndex
  fromWord 11 = Just TessLevelOuter
  fromWord 12 = Just TessLevelInner
  fromWord 13 = Just TessCoord
  fromWord 14 = Just PatchVertices
  fromWord 15 = Just FragCoord
  fromWord 16 = Just PointCoord
  fromWord 17 = Just FrontFacing
  fromWord 18 = Just SampleId
  fromWord 19 = Just SamplePosition
  fromWord 20 = Just SampleMask
  fromWord 22 = Just FragDepth
  fromWord 23 = Just HelperInvocation
  fromWord 24 = Just NumWorkgroups
  fromWord 25 = Just WorkgroupSize
  fromWord 26 = Just WorkgroupId
  fromWord 27 = Just LocalInvocationId
  fromWord 28 = Just GlobalInvocationId
  fromWord 29 = Just LocalInvocationIndex
  fromWord 30 = Just WorkDim
  fromWord 31 = Just GlobalSize
  fromWord 32 = Just EnqueuedWorkgroupSize
  fromWord 33 = Just GlobalOffset
  fromWord 34 = Just GlobalLinearId
  fromWord 36 = Just SubgroupSize
  fromWord 37 = Just SubgroupMaxSize
  fromWord 38 = Just NumSubgroups
  fromWord 39 = Just NumEnqueuedSubgroups
  fromWord 40 = Just SubgroupId
  fromWord 41 = Just SubgroupLocalInvocationId
  fromWord 42 = Just VertexIndex
  fromWord 43 = Just InstanceIndex
  fromWord 4416 = Just SubgroupEqMaskKHR
  fromWord 4417 = Just SubgroupGeMaskKHR
  fromWord 4418 = Just SubgroupGtMaskKHR
  fromWord 4419 = Just SubgroupLeMaskKHR
  fromWord 4420 = Just SubgroupLtMaskKHR
  fromWord 4424 = Just BaseVertex
  fromWord 4425 = Just BaseInstance
  fromWord 4426 = Just DrawIndex
  fromWord _ = Nothing

  requiredCapabilities Position = [Capability.Shader]
  requiredCapabilities PointSize = [Capability.Shader]
  requiredCapabilities ClipDistance = [Capability.ClipDistance]
  requiredCapabilities CullDistance = [Capability.CullDistance]
  requiredCapabilities VertexId = [Capability.Shader]
  requiredCapabilities InstanceId = [Capability.Shader]
  requiredCapabilities PrimitiveId = [Capability.GeometryTessellation]
  requiredCapabilities InvocationId = [Capability.GeometryTessellation]
  requiredCapabilities Layer = [Capability.Geometry]
  requiredCapabilities ViewportIndex = [Capability.MultiViewport]
  requiredCapabilities TessLevelOuter = [Capability.Tessellation]
  requiredCapabilities TessLevelInner = [Capability.Tessellation]
  requiredCapabilities TessCoord = [Capability.Tessellation]
  requiredCapabilities PatchVertices = [Capability.Tessellation]
  requiredCapabilities FragCoord = [Capability.Shader]
  requiredCapabilities PointCoord = [Capability.Shader]
  requiredCapabilities FrontFacing = [Capability.Shader]
  requiredCapabilities SampleId = [Capability.SampleRateShading]
  requiredCapabilities SamplePosition = [Capability.SampleRateShading]
  requiredCapabilities SampleMask = [Capability.SampleRateShading]
  requiredCapabilities FragDepth = [Capability.Shader]
  requiredCapabilities HelperInvocation = [Capability.Shader]
  requiredCapabilities WorkDim = [Capability.Kernel]
  requiredCapabilities GlobalSize = [Capability.Kernel]
  requiredCapabilities EnqueuedWorkgroupSize = [Capability.Kernel]
  requiredCapabilities GlobalOffset = [Capability.Kernel]
  requiredCapabilities GlobalLinearId = [Capability.Kernel]
  requiredCapabilities SubgroupSize = [Capability.Kernel]
  requiredCapabilities SubgroupMaxSize = [Capability.Kernel]
  requiredCapabilities NumSubgroups = [Capability.Kernel]
  requiredCapabilities NumEnqueuedSubgroups = [Capability.Kernel]
  requiredCapabilities SubgroupId = [Capability.Kernel]
  requiredCapabilities SubgroupLocalInvocationId = [Capability.Kernel]
  requiredCapabilities VertexIndex = [Capability.Shader]
  requiredCapabilities InstanceIndex = [Capability.Shader]
  requiredCapabilities SubgroupEqMaskKHR = [Capability.SubgroupBallotKHR]
  requiredCapabilities SubgroupGeMaskKHR = [Capability.SubgroupBallotKHR]
  requiredCapabilities SubgroupGtMaskKHR = [Capability.SubgroupBallotKHR]
  requiredCapabilities SubgroupLeMaskKHR = [Capability.SubgroupBallotKHR]
  requiredCapabilities SubgroupLtMaskKHR = [Capability.SubgroupBallotKHR]
  requiredCapabilities BaseVertex = [Capability.DrawParameters]
  requiredCapabilities BaseInstance = [Capability.DrawParameters]
  requiredCapabilities DrawIndex = [Capability.DrawParameters]
  requiredCapabilities _ = []
