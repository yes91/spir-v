{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.FunctionControl where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | This value is a mask; it can be formed by combining the bits from multiple rows in the table below.
--
-- Used by __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpFunction OpFunction>__.
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_function_control_a_function_control Source for Function Control>
data FunctionControl =
  
    None
    -- | Strong request, to the extent possible, to inline the function.
  | Inline
    -- | Strong request, to the extent possible, to not inline the function.
  | DontInline
    -- | Compiler can assume this function has no side effect, but might read global memory or read through dereferenced function parameters. Always computes the same result for the same argument values.
  | Pure
    -- | Compiler can assume this function has no side effects, and will not access global memory or dereference function parameters. Always computes the same result for the same argument values.
  | Const
  deriving(Read, Show, Eq, Ord)

instance SpirEnum FunctionControl Word32 where
  toWord None = 0
  toWord Inline = 1
  toWord DontInline = 2
  toWord Pure = 4
  toWord Const = 8

  fromWord 0 = Just None
  fromWord 1 = Just Inline
  fromWord 2 = Just DontInline
  fromWord 4 = Just Pure
  fromWord 8 = Just Const
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
