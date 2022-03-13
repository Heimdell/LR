{- |
  Untyped wrapper for reducer functions.
-}
module LR1.Func where

import GHC.Types (Any)
import Unsafe.Coerce (unsafeCoerce)

{- |
  Wrap a function and list of argument manipulators.

  In the rules, we want to be able to drop arguments, make them optional and stuff;
  However, then you need to turn @Maybe X -> Y@ function into pair of @Y@ and @X -> Y@.

  This is why we have `Act` - argument manipulators.
-}
data T = Func Any [Act]

instance Show LR1.Func.T where
  show (Func _ args) = show =<< args

{- |
  Argument manipulators.
-}
data Act
  = Id     -- ^ Use argument as-is.
  | No     -- ^ Insert `Nothing` as next argument.
  | None   -- ^ Insert `[]` as next argument.
  | FJust  -- ^ Use next argument wrapped in `Just`.
  | Drop   -- ^ Ignore next argument.

instance Show Act where
  show = \case
    Id -> "."
    No -> "-"
    None -> "0"
    FJust -> "+"
    Drop -> "_"

{- |
  Neutral to equality.
-}
instance Eq T where
  (==) = const $ const True

{- |
  Neutral to ordering.
-}
instance Ord T where
  compare = const $ const EQ

{- |
  Run a function on a list of inputs.

  If the signature scares the hell out of you - that's because it should.

  This is used in the parser driver module. If you have bypassed the typed
  interface, you can potentially indirecly use that to kill the runtime.
-}
call :: T -> [a] -> b
call (Func f _) [] = unsafeCoerce f
call (Func f (Id    : rest)) (a : as) = call (Func (f `unsafeCoerce` a) rest) as
call (Func f (No    : rest)) (a : as) = call (Func (f `unsafeCoerce` Nothing) rest) (a : as)
call (Func f (None  : rest)) (a : as) = call (Func (f `unsafeCoerce` []) rest) (a : as)
call (Func f (FJust : rest)) (a : as) = call (Func (f `unsafeCoerce` Just a) rest) as
call (Func f (Drop  : rest)) (_ : as) = call (Func f rest) as
call _ _ = error "?"

{- |
  Wrap a function.
-}
func :: a -> [Act] -> LR1.Func.T
func = Func . unsafeCoerce
