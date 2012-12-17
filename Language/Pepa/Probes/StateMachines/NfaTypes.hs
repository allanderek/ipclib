{-|
   A simple module defining the types used for the intermediate
   representation of probes, that is finite automata.
   Here we define the type-classes for states and labels.
-}
module Language.Pepa.Probes.StateMachines.NfaTypes
   ( Move     ( .. )
   , Nfa      ( .. )

   , NfaState ( .. )
   , NfaLabel ( .. )

   , isMoveFrom
   , isEpsilonMove
   , moveTarget
   )
where

{- Standard Libraries Imported -}
import Prelude hiding
   ( map )
import Data.Set
   ( Set
   , singleton
   , map
   )
{- Local Modules Imported -}
import Language.Pepa.QualifiedName
  ( QualifiedName 
  , hprintQualifiedName
  )
{- End of module imports -}


{-|
   The type of a move within a finite state machine.
-}
data (NfaState state, NfaLabel label) => 
      Move state label = Move state label state
                       | Emove state state
                         deriving (Eq,Ord,Show)

{-|
   The type of a non-deterministic finite state machine.
   Also the type of deterministic state machines, this would
   be quite nice to distinguish if I'm honest.
-}

data (NfaState state, NfaLabel label) => Nfa state label = 
   NFA (Set state) 
   (Set (Move state label))
   state
   (Set state)
   deriving (Eq,Show)

{-| Returns true if the given move is from the given state -}
isMoveFrom :: (NfaState state, NfaLabel label) => 
               state -> Move state label -> Bool
isMoveFrom s1 (Move s2 _ _) = s1 == s2
isMoveFrom s1 (Emove s2 _ ) = s1 == s2

{-| Returns true if the move is an epsilon move -}
isEpsilonMove :: (NfaState state, NfaLabel label) => Move state label -> Bool
isEpsilonMove (Emove _ _)  = True
isEpsilonMove (Move _ _ _) = False

{-| Returns the target of a move -}
moveTarget :: (NfaState state, NfaLabel label) => Move state label -> state
moveTarget (Emove _ s)  = s
moveTarget (Move _ _ s) = s

{-| The type class required of labels within an Nfa -}
class Ord a => NfaLabel a where
  stringOfLabel :: a -> String

instance NfaLabel QualifiedName where
  stringOfLabel = hprintQualifiedName

{-| The type class required of states within an Nfa -}
class Ord a => NfaState a where
   minimal  :: a
   renumber :: Int -> a -> a

instance NfaState Int where
   minimal  = 0
   renumber = (+)

instance NfaState a => NfaState (Set a) where
   minimal  = singleton minimal -- or just empty ??
   renumber = renumberStateSet

renumberStateSet :: NfaState state => Int -> Set state -> Set state
renumberStateSet = map . renumber


