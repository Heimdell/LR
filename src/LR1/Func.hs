module LR1.Func where

import GHC.Types (Any)
import Unsafe.Coerce (unsafeCoerce)

data T = Func Any [Act]

instance Show LR1.Func.T where
  show (Func _ args) = show =<< args

data Act
  = Id
  | No
  | None
  | FJust
  | Drop

instance Show Act where
  show = \case
    Id -> "."
    No -> "-"
    None -> "0"
    FJust -> "+"
    Drop -> "_"

instance Eq T where
  (==) = const $ const True

instance Ord T where
  compare = const $ const EQ

call :: T -> [a] -> b
call (Func f _) [] = unsafeCoerce f
call (Func f (Id    : rest)) (a : as) = call (Func (f `unsafeCoerce` a) rest) as
call (Func f (No    : rest)) (a : as) = call (Func (f `unsafeCoerce` Nothing) rest) (a : as)
call (Func f (None  : rest)) (a : as) = call (Func (f `unsafeCoerce` []) rest) (a : as)
call (Func f (FJust : rest)) (a : as) = call (Func (f `unsafeCoerce` Just a) rest) as
call (Func f (Drop  : rest)) (_ : as) = call (Func f rest) as
call _ _ = error "?"

func :: a -> [Act] -> LR1.Func.T
func = Func . unsafeCoerce
