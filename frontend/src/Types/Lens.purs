module Types.Lens where

import Data.Foldable (class Foldable, find)
import Data.Lens (Prism', prism')
import Data.Newtype (class Newtype, unwrap)
import Prelude (class Applicative, pure, (<<<), (==))

-- | Compares equality for a Newtype that has an id with the value of that id.
_withId
  :: forall f fs n
   . Newtype n { id :: Int | fs }
  => Foldable f
  => Applicative f
  => Int
  -> Prism' (f n) n
_withId i
  = prism' pure (find ((\a -> a.id == i) <<< unwrap))
