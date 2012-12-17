{-|
   A module providing the transformation on finite automata
   which removes any non-determinism.
-}
module Language.Pepa.Probes.StateMachines.NfaToDfa 
   ( makeDeterministic )
where


{- Standard Library Modules Imported -}
import Prelude hiding
   ( map )
import Data.Set
   ( Set
   , toList
   , map
   , union
   , singleton
   , empty
   , intersection
   )
{- Local Modules Imported -}
import Language.Pepa.Probes.StateMachines.NfaTypes
   ( Nfa        ( .. )
   , Move       ( .. )
   , NfaState   ( .. )
   , NfaLabel
   )
import Language.Pepa.Probes.StateMachines.NfaLib
   ( alphabet
   , closure
   , onetrans
   )
{- End of module imports -}

{-| Removes any non-determinism from the given nfa -}
makeDeterministic :: (NfaLabel label) =>
                     Nfa Int label -> Nfa Int label
makeDeterministic = number . makeDeter

-------------------------------------------------------------------------- 
--									--
--	number will make an (Nfa Int l) from an Nfa (Set Int) l.        --
--									--
--	Extract a list of the states of the machine (statelist),	--
--	then replace each state by its position in the statelist, 	--
--	given by the function change. These replacements are performed	--
--	by means of the mapset operation.				--
--									--
-------------------------------------------------------------------------- 

number :: (NfaLabel label) => 
          Nfa (Set Int) label -> Nfa Int label
number (NFA states moves start finish)
  = NFA states' moves' start' finish'
    where
    statelist = toList states

    look _n [] _a = error "look in number of NfaToDfa"
    look n (b:y) a 
      | (b==a)      = n
      | otherwise   = look (n+1) y a
    change = look 0 statelist
    states' = map change states
    moves'  = map newmove moves
              where
              newmove (Move s c t) = Move (change s) c (change t) 
              newmove (Emove s t)  = Emove (change s) (change t)
    start' = change start
    finish' = map change finish

-------------------------------------------------------------------------- 
--									--
--	make_deter calls the crucial function 				--
--		deterministic						--
--	on a machine and its alphabet.					--
--									--
-------------------------------------------------------------------------- 

makeDeter :: (NfaLabel label) => 
              Nfa Int label -> Nfa (Set Int) label
makeDeter mach = deterministic mach (alphabet mach)

-------------------------------------------------------------------------- 
--									--
--	deterministic mach alpha					--
--									--
--	is the result of forming the dfa based on sets of states of	--
--	mach, using the alphabet alpha.					--
--									--
--	Calculated by taking the limit of the function addstep		--
--	which adds all the ststes accessible by one transition on	--
--	one of the characters of the alphabet.				--
--	The starting machine has one (start) state, the closure of the	--
--	start state of mach. Note that this may be terminal - test	--
--	for this by taking an intersection of this state set with	--
--	the set term of terminal states of mach.			--
--									--
-------------------------------------------------------------------------- 

deterministic :: (NfaLabel label) =>
                 Nfa Int label -> [ label ] -> Nfa (Set Int) label

deterministic mach alpha 
    = nfaLimit (addstep mach alpha) startmach
      where
      startmach = NFA 
                  (singleton starter)
                  empty
                  starter
                  finish
      starter = closure mach (singleton start)
      finish  
        | (term `intersection` starter) == empty     = empty
        | otherwise                           = singleton starter
      (NFA _sts _mvs start term) = mach

{-
  Addstep adds all the new states which can be made by a single
  transition on one of the characters of the alphabet.
-}
addstep :: (NfaLabel label ) => 
           Nfa Int label -> [ label ]
        -> Nfa (Set Int) label -> Nfa (Set Int) label
addstep mach alpha dfa = 
   addAux dfa $toList states
   where
   (NFA states _m _s _f) = dfa
   addAux =
     foldl (\df st -> addmoves mach st alpha df)

   {-
   addAux dfa1 []        = dfa1
   addAux dfa1 (st:rest) = 
      addAux (addmoves mach st alpha dfa1) rest
   -}

-------------------------------------------------------------------------- 
--									--
--	addmoves mach x alpha dfa					--
--									--
--	will add to dfa all the moves from state set x over alphabet 	--
--	alpha.								--
--	Defined by iterating addmove.					--
--									--
-------------------------------------------------------------------------- 

addmoves :: (NfaLabel label) =>
            Nfa Int label -> Set Int -> [ label ] 
         -> Nfa (Set Int) label -> Nfa (Set Int) label
addmoves _mach _x []    dfa   = dfa
addmoves mach   x (c:r) dfa   = addmoves mach x r $ addmove mach x c dfa

-------------------------------------------------------------------------- 
--									--
--	addmove mach x c dfa						--
--									--
--	will add to dfa the moves from state set x on character c.	--
--									--
-------------------------------------------------------------------------- 
-- So basically if we cannot move out of this state, then 'onetrans' will
-- return the empty set of states, but essentially I do not want that to 
-- be one of my states so I will just ignore it.
addmove :: (NfaLabel label) =>
           Nfa Int label -> Set Int -> label
        -> Nfa (Set Int) label -> Nfa (Set Int) label
addmove mach x c original@(NFA states moves start finish)
   | new == empty = original
   | otherwise    = NFA states' moves' start finish'
   where 
   states' = states `union` (singleton new)
   moves'  = moves  `union` (singleton (Move x c new))
   finish' 
     | empty /= (term `intersection` new)    = finish `union` (singleton new)
     | otherwise                             = finish
   new = onetrans mach c x
   (NFA _s _m _q term) = mach


{-
  Finding the limit of an nfa transforming function. 
  Just like limit except for the change of equality test.
-}
nfaLimit :: (NfaState state, NfaLabel label) =>
             (Nfa state label -> Nfa state label)
          -> Nfa state label -> Nfa state label

nfaLimit f n 
  | (nfa_eq n next) = n
  | otherwise       = 
    nfaLimit f next
    where
    next = f n
    nfa_eq (NFA s1 n1 st1 f1) (NFA s2 n2 st2 f2) = 
      s1 == s2 && n1 == n2 && st1 == st2 && f1 == f2
