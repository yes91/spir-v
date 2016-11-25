{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ExecutionMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Declare the modes an <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#EntryPoint entry point> will execute in. Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpExecutionMode OpExecutionMode>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_execution_mode_a_execution_mode Source for Execution Mode>
data ExecutionMode =
    -- | Number of times to invoke the geometry stage for each input primitive received. The default is to run once for each input primitive. If greater than the target-dependent maximum, it will fail to compile. Only valid with the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
    Invocations
    -- | Requests the tessellation primitive generator to divide edges into a collection of equal-sized segments. Only valid with one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | SpacingEqual
    -- | Requests the tessellation primitive generator to divide edges into an even number of equal-length segments plus two additional shorter fractional segments. Only valid with one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | SpacingFractionalEven
    -- | Requests the tessellation primitive generator to divide edges into an odd number of equal-length segments plus two additional shorter fractional segments. Only valid with one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | SpacingFractionalOdd
    -- | Requests the tessellation primitive generator to generate triangles in clockwise order. Only valid with one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | VertexOrderCw
    -- | Requests the tessellation primitive generator to generate triangles in counter-clockwise order. Only valid with one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | VertexOrderCcw
    -- | Pixels appear centered on whole-number pixel offsets. E.g., the coordinate (0.5, 0.5) appears to move to (0.0, 0.0). Only valid with the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>. If a __Fragment__ entry point does not have this set, pixels appear centered at offsets of (0.5, 0.5) from whole numbers
  | PixelCenterInteger
    -- | Pixel coordinates appear to originate in the upper left, and increase toward the right and downward. Only valid with the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | OriginUpperLeft
    -- | Pixel coordinates appear to originate in the lower left, and increase toward the right and upward. Only valid with the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | OriginLowerLeft
    -- | Fragment tests are to be performed before fragment shader execution. Only valid with the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | EarlyFragmentTests
    -- | Requests the tessellation primitive generator to generate a point for each distinct vertex in the subdivided primitive, rather than to generate lines or triangles. Only valid with one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | PointMode
    -- | This stage will run in transform feedback-capturing mode and this module is responsible for describing the transform-feedback setup. See the __XfbBuffer__, __Offset__, and __XfbStride__ __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Decoration Decorations>__.
  | Xfb
    -- | This mode must be declared if this module potentially changes the fragment’s depth. Only valid with the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | DepthReplacing
    -- | External optimizations may assume depth modifications will leave the fragment’s depth as greater than or equal to the fragment’s interpolated depth value (given by the /z/ component of the __FragCoord__ __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#BuiltIn BuiltIn>__ decorated variable). Only valid with the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | DepthGreater
    -- | External optimizations may assume depth modifications leave the fragment’s depth less than the fragment’s interpolated depth value, (given by the /z/ component of the __FragCoord__ __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#BuiltIn BuiltIn>__ decorated variable). Only valid with the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | DepthLess
    -- | External optimizations may assume this stage did not modify the fragment’s depth. However, __DepthReplacing__ mode must accurately represent depth modification. Only valid with the __Fragment__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | DepthUnchanged
    -- | Indicates the work-group size in the /x/, /y/, and /z/ dimensions. Only valid with the __GLCompute__ or __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | LocalSize
    -- | A hint to the compiler, which indicates the most likely to be used work-group size in the /x/, /y/, and /z/ dimensions. Only valid with the __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | LocalSizeHint
    -- | Stage input primitive is /points/. Only valid with the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | InputPoints
    -- | Stage input primitive is /lines/. Only valid with the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | InputLines
    -- | Stage input primitive is /lines adjacency/. Only valid with the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | InputLinesAdjacency
    -- | For a geometry stage, input primitive is /triangles/. For a tessellation stage, requests the tessellation primitive generator to generate triangles. Only valid with the __Geometry__ or one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | Triangles
    -- | Geometry stage input primitive is /triangles adjacency/. Only valid with the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | InputTrianglesAdjacency
    -- | Requests the tessellation primitive generator to generate /quads/. Only valid with one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | Quads
    -- | Requests the tessellation primitive generator to generate /isolines/. Only valid with one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | Isolines
    -- | For a geometry stage, the maximum number of vertices the shader will ever emit in a single <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocation>. For a tessellation-control stage, the number of vertices in the output patch produced by the tessellation control shader, which also specifies the number of times the tessellation control shader is invoked. Only valid with the __Geometry__ or one of the tessellation <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Models>.
  | OutputVertices
    -- | Stage output primitive is /points/. Only valid with the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | OutputPoints
    -- | Stage output primitive is /line strip/. Only valid with the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | OutputLineStrip
    -- | Stage output primitive is /triangle strip/. Only valid with the __Geometry__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | OutputTriangleStrip
    -- | A hint to the compiler, which indicates that most operations used in the entry point are explicitly vectorized using a particular vector type. The 16 high-order bits of /Vector Type/ operand specify the /number of components/ of the vector. The 16 low-order bits of /Vector Type/ operand specify the /data type/ of the vector.
    --
    --  These are the legal /data type/ values:
    --
    --  /0/ represents an 8-bit integer value.
    --
    --  /1/ represents a 16-bit integer value.
    --
    --  /2/ represents a 32-bit integer value.
    --
    --  /3/ represents a 64-bit integer value.
    --
    --  /4/ represents a 16-bit float value.
    --
    --  /5/ represents a 32-bit float value.
    --
    --  /6/ represents a 64-bit float value.
    --
    --  Only valid with the __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | VecTypeHint
    -- | Indicates that floating-point-expressions contraction is disallowed. Only valid with the __Kernel__ <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Execution_Model Execution Model>.
  | ContractionOff
    -- | Indicates that this entry point is a module initializer.
  | Initializer
    -- | Indicates that this entry point is a module finalizer.
  | Finalizer
    -- | Indicates that this entry point requires the specified /Subgroup Size/.
  | SubgroupSize
    -- | Indicates that this entry point requires the specified number of /Subgroups Per Workgroup/.
  | SubgroupsPerWorkgroup
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ExecutionMode Word32 where
  toWord Invocations = 0
  toWord SpacingEqual = 1
  toWord SpacingFractionalEven = 2
  toWord SpacingFractionalOdd = 3
  toWord VertexOrderCw = 4
  toWord VertexOrderCcw = 5
  toWord PixelCenterInteger = 6
  toWord OriginUpperLeft = 7
  toWord OriginLowerLeft = 8
  toWord EarlyFragmentTests = 9
  toWord PointMode = 10
  toWord Xfb = 11
  toWord DepthReplacing = 12
  toWord DepthGreater = 14
  toWord DepthLess = 15
  toWord DepthUnchanged = 16
  toWord LocalSize = 17
  toWord LocalSizeHint = 18
  toWord InputPoints = 19
  toWord InputLines = 20
  toWord InputLinesAdjacency = 21
  toWord Triangles = 22
  toWord InputTrianglesAdjacency = 23
  toWord Quads = 24
  toWord Isolines = 25
  toWord OutputVertices = 26
  toWord OutputPoints = 27
  toWord OutputLineStrip = 28
  toWord OutputTriangleStrip = 29
  toWord VecTypeHint = 30
  toWord ContractionOff = 31
  toWord Initializer = 33
  toWord Finalizer = 34
  toWord SubgroupSize = 35
  toWord SubgroupsPerWorkgroup = 36

  fromWord 0 = Just Invocations
  fromWord 1 = Just SpacingEqual
  fromWord 2 = Just SpacingFractionalEven
  fromWord 3 = Just SpacingFractionalOdd
  fromWord 4 = Just VertexOrderCw
  fromWord 5 = Just VertexOrderCcw
  fromWord 6 = Just PixelCenterInteger
  fromWord 7 = Just OriginUpperLeft
  fromWord 8 = Just OriginLowerLeft
  fromWord 9 = Just EarlyFragmentTests
  fromWord 10 = Just PointMode
  fromWord 11 = Just Xfb
  fromWord 12 = Just DepthReplacing
  fromWord 14 = Just DepthGreater
  fromWord 15 = Just DepthLess
  fromWord 16 = Just DepthUnchanged
  fromWord 17 = Just LocalSize
  fromWord 18 = Just LocalSizeHint
  fromWord 19 = Just InputPoints
  fromWord 20 = Just InputLines
  fromWord 21 = Just InputLinesAdjacency
  fromWord 22 = Just Triangles
  fromWord 23 = Just InputTrianglesAdjacency
  fromWord 24 = Just Quads
  fromWord 25 = Just Isolines
  fromWord 26 = Just OutputVertices
  fromWord 27 = Just OutputPoints
  fromWord 28 = Just OutputLineStrip
  fromWord 29 = Just OutputTriangleStrip
  fromWord 30 = Just VecTypeHint
  fromWord 31 = Just ContractionOff
  fromWord 33 = Just Initializer
  fromWord 34 = Just Finalizer
  fromWord 35 = Just SubgroupSize
  fromWord 36 = Just SubgroupsPerWorkgroup
  fromWord _ = Nothing

  requiredCapabilities Invocations = [Capability.Geometry]
  requiredCapabilities SpacingEqual = [Capability.Tessellation]
  requiredCapabilities SpacingFractionalEven = [Capability.Tessellation]
  requiredCapabilities SpacingFractionalOdd = [Capability.Tessellation]
  requiredCapabilities VertexOrderCw = [Capability.Tessellation]
  requiredCapabilities VertexOrderCcw = [Capability.Tessellation]
  requiredCapabilities PixelCenterInteger = [Capability.Shader]
  requiredCapabilities OriginUpperLeft = [Capability.Shader]
  requiredCapabilities OriginLowerLeft = [Capability.Shader]
  requiredCapabilities EarlyFragmentTests = [Capability.Shader]
  requiredCapabilities PointMode = [Capability.Tessellation]
  requiredCapabilities Xfb = [Capability.TransformFeedback]
  requiredCapabilities DepthReplacing = [Capability.Shader]
  requiredCapabilities DepthGreater = [Capability.Shader]
  requiredCapabilities DepthLess = [Capability.Shader]
  requiredCapabilities DepthUnchanged = [Capability.Shader]
  requiredCapabilities LocalSizeHint = [Capability.Kernel]
  requiredCapabilities InputPoints = [Capability.Geometry]
  requiredCapabilities InputLines = [Capability.Geometry]
  requiredCapabilities InputLinesAdjacency = [Capability.Geometry]
  requiredCapabilities Triangles = [Capability.GeometryTessellation]
  requiredCapabilities InputTrianglesAdjacency = [Capability.Geometry]
  requiredCapabilities Quads = [Capability.Tessellation]
  requiredCapabilities Isolines = [Capability.Tessellation]
  requiredCapabilities OutputVertices = [Capability.GeometryTessellation]
  requiredCapabilities OutputPoints = [Capability.Geometry]
  requiredCapabilities OutputLineStrip = [Capability.Geometry]
  requiredCapabilities OutputTriangleStrip = [Capability.Geometry]
  requiredCapabilities VecTypeHint = [Capability.Kernel]
  requiredCapabilities ContractionOff = [Capability.Kernel]
  requiredCapabilities Initializer = [Capability.Kernel]
  requiredCapabilities Finalizer = [Capability.Kernel]
  requiredCapabilities SubgroupSize = [Capability.SubgroupDispatch]
  requiredCapabilities SubgroupsPerWorkgroup = [Capability.SubgroupDispatch]
  requiredCapabilities _ = []
