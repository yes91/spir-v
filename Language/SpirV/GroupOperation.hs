{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.GroupOperation where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability


-- | Defines the class of workgroup or subgroup operation. Used by:
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupIAdd OpGroupIAdd>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFAdd OpGroupFAdd>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFMin OpGroupFMin>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupUMin OpGroupUMin>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupSMin OpGroupSMin>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupFMax OpGroupFMax>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupUMax OpGroupUMax>__
--
-- -   __<https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#OpGroupSMax OpGroupSMax>__
--
-- <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#_a_id_group_operation_a_group_operation Source for Group Operation>
data GroupOperation =
    -- | A reduction operation for all values of a specific value X specified by <https:\/\/www.khronos.org\/registry\/spir-v\/specs\/1.0\/SPIRV.html#Invocation invocations> within a workgroup.
    Reduce
    -- | A binary operation with an identity /I/ and /n/ (where /n/ is the size of the workgroup) elements[/a0/, /a1/, … /an-1/] resulting in [/a0/, (/a0/ op /a1/), …(/a0/ op /a1/ op … op /an-1/)]
  | InclusiveScan
    -- | A binary operation with an identity /I/ and /n/ (where /n/ is the size of the workgroup) elements[/a0/, /a1/, … /an-1/] resulting in [/I/, /a0/, (/a0/ op /a1/), … (/a0/ op /a1/ op … op /an-2/)].
  | ExclusiveScan
  deriving(Read, Show, Eq, Ord)

instance SpirEnum GroupOperation Word32 where
  toWord Reduce = 0
  toWord InclusiveScan = 1
  toWord ExclusiveScan = 2

  fromWord 0 = Just Reduce
  fromWord 1 = Just InclusiveScan
  fromWord 2 = Just ExclusiveScan
  fromWord _ = Nothing

  requiredCapabilities Reduce = [Capability.Kernel]
  requiredCapabilities InclusiveScan = [Capability.Kernel]
  requiredCapabilities ExclusiveScan = [Capability.Kernel]
  
