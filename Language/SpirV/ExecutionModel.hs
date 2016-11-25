{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ExecutionModel where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpEntryPoint OpEntryPoint>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_execution_model_a_execution_model Source for Execution Model>
data ExecutionModel =
    -- | Vertex shading stage.
    Vertex
    -- | Tessellation control (or hull) shading stage.
  | TessellationControl
    -- | Tessellation evaluation (or domain) shading stage.
  | TessellationEvaluation
    -- | Geometry shading stage.
  | Geometry
    -- | Fragment shading stage.
  | Fragment
    -- | Graphical compute shading stage.
  | GLCompute
    -- | Compute kernel.
  | Kernel
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ExecutionModel Word32 where
  toWord Vertex = 0
  toWord TessellationControl = 1
  toWord TessellationEvaluation = 2
  toWord Geometry = 3
  toWord Fragment = 4
  toWord GLCompute = 5
  toWord Kernel = 6

  fromWord 0 = Just Vertex
  fromWord 1 = Just TessellationControl
  fromWord 2 = Just TessellationEvaluation
  fromWord 3 = Just Geometry
  fromWord 4 = Just Fragment
  fromWord 5 = Just GLCompute
  fromWord 6 = Just Kernel
  fromWord _ = Nothing

  requiredCapabilities Vertex = [Capability.Shader]
  requiredCapabilities TessellationControl = [Capability.Tessellation]
  requiredCapabilities TessellationEvaluation = [Capability.Tessellation]
  requiredCapabilities Geometry = [Capability.Geometry]
  requiredCapabilities Fragment = [Capability.Shader]
  requiredCapabilities GLCompute = [Capability.Shader]
  requiredCapabilities Kernel = [Capability.Kernel]
  
