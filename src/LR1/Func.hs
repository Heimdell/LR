{- |
  Untyped wrapper for reducer functions.
-}
module LR1.Func where

import Data.Dynamic
import Data.Proxy
import qualified Data.List.NonEmpty as NE
import Data.Typeable (typeRep)
import Debug.Trace

{- |
  Wrap a function and list of argument manipulators.

  In the rules, we want to be able to drop arguments, make them optional and stuff;
  However, then you need to turn @Maybe X -> Y@ function into pair of @Y@ and @X -> Y@.

  This is why we have `Act` - argument manipulators.
-}
data T = Func Dynamic [Act]

instance Show LR1.Func.T where
  show (Func _ args) = show =<< args

{- |
  Argument manipulators.
-}
data Act where
  Id :: Act     -- ^ Use argument as-is.
  No :: Typeable a => Proxy a -> Act     -- ^ Insert `Nothing` as next argument.
  None :: Typeable a => Proxy a -> Act   -- ^ Insert `[]` as next argument.
  Some :: Typeable a => Proxy a -> Act   -- ^ Insert `[]` as next argument.
  FJust :: Act  -- ^ Use next argument wrapped in `Just`.
  Drop :: Act  -- ^ Ignore next argument.

instance Show Act where
  show = \case
    Id -> "."
    No _ -> "-"
    None _ -> "0"
    Some _ -> "1"
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
call :: T -> [Dynamic] -> Dynamic
call (Func f _) [] = trace "End"  f
call (Func f (Id                  : rest)) (a : as) = trace "Id"    $  call (Func (f `dynApp` a)                          rest) as
call (Func f (No   (_ :: Proxy a) : rest)) (a : as) = trace "No"    $  call (Func (f `dynApp` toDyn (Nothing :: Maybe a)) rest) (a : as)
call (Func f (None (_ :: Proxy a) : rest)) (a : as) = trace "None"  $  call (Func (f `dynApp` toDyn ([] :: [a]))          rest) (a : as)
call (Func f (Some (_ :: Proxy a) : rest)) (a : as) = trace "Some"  $  call (Func (f `dynApp` toDyn (neToList a :: [a]))  rest) (a : as)
call (Func f (FJust               : rest)) (a : as) = trace "FJust" $  call (Func (f `dynApp` toDyn (Just a))             rest)      as
call (Func f (Drop                : rest)) (_ : as) = trace "Drop"  $  call (Func  f                                      rest)      as
call _ _ = error "?"

{- |
  Wrap a function.
-}

neToList :: forall a. Typeable a => Dynamic -> [a]
neToList a = NE.toList $ fromDyn a $ error msg
  where
    msg = "Expected " <> show (typeRep (Proxy :: Proxy (NE.NonEmpty a))) <> ", got " <> show (dynTypeRep a)

fromDynUnsafe :: forall a. Typeable a => Dynamic -> a
fromDynUnsafe a = fromDyn a $ error msg
  where
    msg = "Expected " <> show (typeRep (Proxy :: Proxy a)) <> ", got " <> show (dynTypeRep a)

func :: Typeable a => a -> [Act] -> LR1.Func.T
func f = Func (Data.Dynamic.toDyn f)
