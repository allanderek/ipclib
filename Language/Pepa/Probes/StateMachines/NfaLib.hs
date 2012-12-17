
-------------------------------------------------------------------------- 
--									--
--	NfaLib.hs							--
--									--
--	Useful functions used in the implementation of an NFA and	--
--	the conversion of an NFA to a DFA.				--
--	Therefore used in ImplementNfa and NfaToFDfa.			--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--									--
-------------------------------------------------------------------------- 

module Language.Pepa.Probes.StateMachines.NfaLib
   ( closure
   , onemove
   , onetrans
   , alphabet
   , movesFromState
   , startstate
   )
where

{- Standard Library Modules Imported -}
import Prelude hiding
   ( map
   , filter
   )
import Data.List
   ( nub )
import Data.Set
   ( Set
   , toList
   , fromList
   , union
   , filter
   )
{- Local Modules Imported -}
import Language.Pepa.Probes.StateMachines.NfaTypes
   ( Nfa       ( .. )
   , Move      ( .. )
   
   , NfaState
   , NfaLabel
   )
{- End of module imports -}


-------------------------------------------------------------------------- 
--									--
--	The epsilon closure of a set of states in an NFA. This is 	--
--	found by finding the limit of the function which adds to a	--
--	set all those states accessible by a single epsilon move.	--
--	The limit is found using setlimit.				--
--									--
-------------------------------------------------------------------------- 

closure :: (NfaState state, NfaLabel label) => 
           Nfa state label -> Set state -> Set state
closure (NFA _states moves _start _term) =
   setlimit add
   where
   add stateset = union stateset $ fromList accessible
                  where
                  accessible = [ s | x         <- toList stateset
                                   , Emove y s <- toList moves
                                   , y==x
                               ]

{- I'm sure there is a library function for this, is it
   Control.Mondad.fix?
-}
setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setlimit f s
   | s==next   = s
   | otherwise = setlimit f next
   where
   next = f s

-------------------------------------------------------------------------- 
--									--
--	Onemove finds the set of states accessible from a given set	--
--	by a single move on the given character.			--
--									--
-------------------------------------------------------------------------- 

onemove :: (NfaState state, NfaLabel label) => 
           Nfa state label -> label -> Set state -> Set state
onemove (NFA _states moves _start _term) c x =
   fromList [ s | t <- toList x 
                , Move z d s <- toList moves
                , z==t , c==d
            ]

-------------------------------------------------------------------------- 
--									--
--	Onetrans performs one move (by onemove) and then takes the	--
--	epsilon closure of the result.					--
--									--
-------------------------------------------------------------------------- 

onetrans :: (NfaState state, NfaLabel label) => 
            Nfa state label -> label -> Set state -> Set state
onetrans mach c = closure mach . onemove mach c


-- Finds the set of moves possible from a given state
movesFromState :: (NfaState state, NfaLabel label) => 
                  Nfa state label -> state -> Set (Move state label)
movesFromState (NFA _ moves _ _) state =
   filter fromCurrentState moves
   where
   -- How do I write this type down??
   -- fromCurrentState :: Move state label -> Bool
   fromCurrentState (Move j _ _) = j == state
   fromCurrentState (Emove j _)  = j == state
-------------------------------------------------------------------------- 
--									--
--	Auxilliary functions.						--
--									--
--	startstate	extracts the start state of a machine.		--
--									--
--	alphabet 	returns the alphabet of the machine, by 	--
--			finding a list of the characters mentioned	--
--			in the Moves.					--
--									--
-------------------------------------------------------------------------- 

startstate :: (NfaState state, NfaLabel label) => 
              Nfa state label -> state
startstate (NFA _states _moves start _finish) = start

alphabet :: (NfaState state, NfaLabel label) => 
            Nfa state label -> [ label ]
alphabet (NFA _states moves _st _f) =
   nub [ c | Move _ c _ <- toList moves ]

