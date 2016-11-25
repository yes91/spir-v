{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.SelectionControl where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | This value is a mask; it can be formed by combining the bits from multiple rows in the table below.
--
-- Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpSelectionMerge OpSelectionMerge>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_selection_control_a_selection_control Source for Selection Control>
data SelectionControl =
  
    None
    -- | Strong request, to the extent possible, to remove the control flow for this selection.
  | Flatten
    -- | Strong request, to the extent possible, to keep this selection as control flow.
  | DontFlatten
  deriving(Read, Show, Eq, Ord)

instance SpirEnum SelectionControl Word32 where
  toWord None = 0
  toWord Flatten = 1
  toWord DontFlatten = 2

  fromWord 0 = Just None
  fromWord 1 = Just Flatten
  fromWord 2 = Just DontFlatten
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
