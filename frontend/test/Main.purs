module Test.Main where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (dropWhile, head, sort, tail, zipWith)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, otherwise, show, (&&), (<), (<=), (<>), (==))
import Test.QuickCheck (quickCheck, Result(..))
import Types.Speaker (Speaker(..))

isSuccessful :: Result -> Boolean
isSuccessful Success = true
isSuccessful _       = false

speakersAreSortedCorrectly :: Array Speaker -> Result
speakersAreSortedCorrectly [] = Success
speakersAreSortedCorrectly ss
  = case head (dropWhile isSuccessful (zipWith checkPair ss' (unsafePartial (fromJust (tail ss'))))) of
      Nothing -> Success  -- No errors in the array.
      Just e  -> e        -- The first error in the array.
  where
    ss' = sort ss

    checkPair :: Speaker -> Speaker -> Result
    checkPair (Speaker s1) (Speaker s2)
      | s1.timesSpoken <  s2.timesSpoken                   = Success
      | s1.timesSpoken == s2.timesSpoken && s1.id <= s2.id = Success
      | otherwise = Failed ("Speakers are not correctly sorted for: " <> show (Speaker s1) <> ", " <> show (Speaker s2))

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, random :: RANDOM | e) Unit
main = do
  quickCheck speakersAreSortedCorrectly
