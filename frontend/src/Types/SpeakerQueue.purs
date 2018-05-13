module Types.SpeakerQueue
  ( SpeakerQueue(..)
  , SpeakerQueueRecord
  , _Speaking
  , _Speakers
  , addSpeaker
  , modifySpeaker
  ) where

import Control.Alt ((<|>))
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Lens (Lens', _Just, filtered, lens, over, preview, traverseOf)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Record.ShowRecord (showRecord)
import Prelude (class Applicative, class Eq, class Ord, class Show, compare, id, pure, ($), (&&), (/=), (<#>), (<<<), (<>), (==), (>>=), (>>>))
import Simple.JSON (class ReadForeign, readImpl)
import Types.Lens (_withId)
import Types.Speaker (Speaker(..))

type SpeakerQueueRecord =
  { id       :: Int
  , state    :: String
  , speaking :: Maybe Speaker
  , speakers :: Array Speaker
  }
newtype SpeakerQueue = SpeakerQueue SpeakerQueueRecord
derive instance ntSQ :: Newtype SpeakerQueue _
derive instance eqSQ :: Eq      SpeakerQueue

instance shSQ :: Show SpeakerQueue where
  show (SpeakerQueue sq) = "SpeakerQueue " <> showRecord sq

instance rfSQ :: ReadForeign SpeakerQueue where
  readImpl fr =
    readImpl fr
      <#> over _Speakers A.sort
      >>> over _Speaking id -- Applying the setter for _Speakers and then _Speaking is the same as applying the deprecated invariantDance to a SpeakerQueue.

-- | The speaker queue with the highest id should be on top of the stack.
-- | This will let us work our way down to the first speaker queue by
-- | popping speaker queues from the stack.
instance ordSQ :: Ord SpeakerQueue where
  compare (SpeakerQueue s1) (SpeakerQueue s2) =
    compare s2.id s1.id

-- | The lenses below do some housekeeping, they make sure that no speaker on
-- | the `speaking` position is done.  And that no speaker in the queue is
-- | done or deleted. And that no speaker first in the queue is active.

_Speaking :: Lens' SpeakerQueue (Maybe Speaker)
_Speaking =
  lens
    (\(SpeakerQueue {speaking}) -> speaking)
    (\(SpeakerQueue r) s' -> SpeakerQueue (r { speaking = preview (_Just <<< filtered (\(Speaker s'') -> s''.state /= "done")) s' }))

_Speakers :: Lens' SpeakerQueue (Array Speaker)
_Speakers =
  lens
    (\(SpeakerQueue {speakers}) -> speakers)
    (\(SpeakerQueue r) s' -> SpeakerQueue (moveActive (r { speakers = s' })))

  where
    moveActive sq =
      fromMaybe sq $ A.uncons (A.filter (\(Speaker s) -> s.state /= "done" && s.state /= "deleted") sq.speakers)
        >>= \{head, tail} ->
          let (Speaker s') = head
           in if s'.state == "active"
                then Just (sq { speaking = Just head, speakers = tail })
                else Nothing

addSpeaker :: Speaker -> SpeakerQueue -> SpeakerQueue
addSpeaker s = over _Speakers (A.insert s)

-- | Modify the speaker with the id that is the first argument of this function.
-- | Do nothing if the speaker isn't found.
-- TODO: We should really be able to refactor this.
modifySpeaker :: Int -> (Speaker -> Speaker) -> SpeakerQueue -> SpeakerQueue
modifySpeaker id_ f sq = fromMaybe sq $ go _Speaking sq <|> go _Speakers sq
  where
    go
      :: forall f
       . Applicative f
      => Foldable f
      => Lens' SpeakerQueue (f Speaker)
      -> SpeakerQueue
      -> Maybe SpeakerQueue
    go _l = traverseOf (_l <<< _withId id_) (pure <<< f)
