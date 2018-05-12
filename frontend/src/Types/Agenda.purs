module Types.Agenda
  ( AgendaItem(..)
  , Agenda
  , _AgendaItems
  , _CurrentAgendaItem
  , _SpeakerQueues
  , topSQ
  , pushSQ
  , popSQIfMatchingId
  , modifySQ
  , next
  , prev
  , empty
  , insert
  , modify
  , jumpToFirstActive
  , getCurrentAI
  ) where

import Types.SpeakerQueue

import Control.Alternative ((<|>))
import Data.Array as A
import Data.Either (Either, note)
import Data.Lens (Lens', Prism', filtered, lens, over, preview, prism, to, traverseOf, view)
import Data.List as L
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Record as R
import Data.Record.ShowRecord (showRecord)
import Prelude (class Eq, class Ord, class Show, compare, const, eq, pure, (+), (-), (/=), (<#>), (<$>), (<<<), (<>), (==), (>>=), (>>>))
import Simple.JSON (class ReadForeign, readImpl)
import Type.Prelude (SProxy(..))

------------------------------------------------------------------------------
-- | AgendaItem
------------------------------------------------------------------------------

newtype AgendaItem = AgendaItem
  { id            :: Int
  , supertitle    :: Maybe String
  , title         :: String
  , order_        :: Int
  , state         :: String
  , speakerQueues :: L.List SpeakerQueue
  }
derive instance ntAI :: Newtype AgendaItem _
derive instance eqAI :: Eq      AgendaItem

instance shAI :: Show AgendaItem where
  show (AgendaItem ai) = "AgendaItem " <> showRecord ai

instance ordAI :: Ord AgendaItem where
  compare (AgendaItem a1) (AgendaItem a2) =
    compare a1.order_ a2.order_

-- | `speakerQueues` is a stack and we put the largest element on the top.
-- | This should be excellent.
instance rfAI :: ReadForeign AgendaItem where
  readImpl fr =
    readImpl fr
      <#> R.modify
            (SProxy :: SProxy "speakerQueues")
            (L.sort <<< (L.fromFoldable :: Array SpeakerQueue -> L.List SpeakerQueue))
      >>> AgendaItem

_SpeakerQueues :: Lens' AgendaItem (L.List SpeakerQueue)
_SpeakerQueues
  = lens
      (\(AgendaItem a)    -> a.speakerQueues)
      (\(AgendaItem a) ss -> AgendaItem a { speakerQueues = ss })

-- | The top of the speaker queue stack
topSQ :: AgendaItem -> Maybe SpeakerQueue
topSQ = view (_SpeakerQueues <<< to L.head)

pushSQ :: SpeakerQueue -> AgendaItem -> AgendaItem
pushSQ sq = over _SpeakerQueues (L.Cons sq)

-- | If the ids match, pop the speakerQueue, otherwise, noop.
popSQIfMatchingId :: Int -> AgendaItem -> Maybe AgendaItem
popSQIfMatchingId id (AgendaItem ai)
  =   L.uncons ai.speakerQueues
  >>= \{head, tail} ->
        preview (to unwrap <<< filtered (\sq -> sq.id == id)) head
          <#> const (AgendaItem ai { speakerQueues = tail })

-- | We try to modify the speaker queue with the supplied id.
-- | If the speaker queue isn't found, we give back Nothing.
-- | And if you give us Nothing in the callback, we pass it on.
modifySQ :: Int -> (SpeakerQueue -> Maybe SpeakerQueue) -> AgendaItem -> Maybe AgendaItem
modifySQ i f
  = traverseOf (_SpeakerQueues) (L.span (\(SpeakerQueue s) -> s.id /= i)
    >>> \{init, rest} ->
       L.uncons rest
         >>= \{head, tail} -> f head
         <#> \h' -> init <> L.Cons h' tail
    )

------------------------------------------------------------------------------
-- | Agenda
------------------------------------------------------------------------------
data Agenda = Agenda Int (Array AgendaItem)

instance rfAg :: ReadForeign Agenda where
  readImpl fr = Agenda 0 <$> readImpl fr

-- | How to get insight into the agenda.
_AgendaItems :: Lens' Agenda (Array AgendaItem)
_AgendaItems
  = lens
      (\(Agenda _ items)       -> items)
      (\(Agenda i _    ) items -> Agenda i items)

_CurrentAgendaItem :: Prism' Agenda AgendaItem
_CurrentAgendaItem
  = prism
      (Agenda 0 <<< pure)
      (\(Agenda i as) -> note empty (A.index as i))

-- | Set the internal index to the first argument, and return Just the Agenda.
-- | If out of bounds, return Nothing.
setIdx :: Int -> Agenda -> Maybe Agenda
setIdx i (Agenda _ as) = A.index as i <#> const (Agenda i as)

next :: Agenda -> Maybe Agenda
next a@(Agenda i _) = setIdx (i+1) a

prev :: Agenda -> Maybe Agenda
prev a@(Agenda i _) = setIdx (i-1) a

empty :: Agenda
empty = Agenda 0 []

insert :: AgendaItem -> Agenda -> Agenda
insert ai = over _AgendaItems (A.insert ai)

-- | We try to modify the agenda item with the same id.
-- | If the agenda item isn't found, we give back Nothing.
-- | If you change your mind, just return Nothing in the callback
-- | function and we forget that it all happened.
modify :: Int -> (AgendaItem -> Maybe AgendaItem) -> Agenda -> Maybe Agenda
modify aid f (Agenda c as) =
  A.findIndex (\(AgendaItem a) -> eq aid a.id) as
    >>= \idx -> A.index as idx
    >>= f
    >>= \a -> A.updateAt idx a as
    <#> Agenda c

-- | This is an interesting beast...
jumpToFirstActive :: Agenda -> Agenda
jumpToFirstActive (Agenda _ as) = Agenda i as
  where
    i = fromMaybe ((A.length as) - 1)
      (A.findIndex (\(AgendaItem a) -> a.state == "active") as
      <|> A.findIndex (\(AgendaItem a) -> a.state /= "done") as)

getCurrentAI :: Agenda -> Either String AgendaItem
getCurrentAI
  = note "ERROR: There is no current agenda item." <<< preview _CurrentAgendaItem
