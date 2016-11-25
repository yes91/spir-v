{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.MemoryAccess where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Memory access semantics.
--
-- This value is a mask; it can be formed by combining the bits from multiple rows in the table below.
--
-- Used by:
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpLoad OpLoad>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpStore OpStore>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCopyMemory OpCopyMemory>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpCopyMemorySized OpCopyMemorySized>__
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_memory_access_a_memory_access Source for Memory Access>
data MemoryAccess =
  
    None
    -- | This access cannot be eliminated, duplicated, or combined with other accesses.
  | Volatile
    -- | This access has a known alignment, provided as a literal in the next operand.
  | Aligned
    -- | Hints that the accessed address is not likely to be accessed again in the near future.
  | Nontemporal
  deriving(Read, Show, Eq, Ord)

instance SpirEnum MemoryAccess Word32 where
  toWord None = 0
  toWord Volatile = 1
  toWord Aligned = 2
  toWord Nontemporal = 4

  fromWord 0 = Just None
  fromWord 1 = Just Volatile
  fromWord 2 = Just Aligned
  fromWord 4 = Just Nontemporal
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
