module LR1.Func where

import GHC.Types (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype T = Func Any

call :: T -> [a] -> b
call f [] = unsafeCoerce f
call f (a : as) = call (f `unsafeCoerce` a) as
