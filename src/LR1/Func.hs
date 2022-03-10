module LR1.Func where

import GHC.Types (Any)
import Unsafe.Coerce (unsafeCoerce)

data T = Func Any [Bool]

call :: T -> [a] -> b
call (Func f _) [] = unsafeCoerce f
call (Func f (True : rest)) (a : as) = call (Func (f `unsafeCoerce` a) rest) as
call (Func f (_    : rest)) (_ : as) = call (Func f rest) as
call _ _ = error "?"
