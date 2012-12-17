{-|
  The 'FixMap' module implements a map from keys to elements that
  supports iteration. The idea is that a computation which modifies
  a mapping is repeatedly run until the mapping is not modified.
-}

module Language.Pepa.Analysis.FixMap 
    ( FixMap
    , empty
    , insert
    , insertWith
    , insertChanged
    , insertList
    , lookup
    , toList
    , toMap
    , fixPoint
    )
where

{- Imported Standard Libraries -}
import Prelude hiding ( lookup )
import qualified Data.Map as M
import Data.List      ( union )


{-|
  A finite map is represented as a pair.
  The first element of the pair is a boolean which indicates whether
  or not we have modified the map sinc we last set this to @False@.
  The second element of the pair is the map itself.
  The idea is that before each iteration of a computation the first
  element is set to @False@, we then perform the computation and
  check if the map has been modified by examining the first element
  of the pair.
-}
type FixMap k a = (Bool, M.Map k a)

{-| An 'FixMap' which contains no mappings and has therefore not been modified -}
empty :: FixMap k a
empty = (False, M.empty)


{-|
  Insertion must check if the given key is currently in the
  given map. If it is, then the element to which it is
  mapped to is checked for equality with the given element.
  If they are the same then we are not actually updating the
  map, otherwise we are.
-}
insert :: (Eq e, Ord k) => k -> e -> FixMap k e -> FixMap k e
insert k e fm@(_, m) =
    case M.lookup k m of
    Just e' 
        |  e == e'   -> fm
        |  otherwise -> (True, M.insert k e m)
    Nothing          -> (True, M.insert k e m)


{-|
  The 'insertWith' function is the same as the 'insert' function
  except that a function to test for equality on map elements is
  given. 
-}
insertWith :: (Eq e, Ord k) => 
              (e -> e -> e) -> k -> e -> FixMap k e -> FixMap k e
insertWith f k e fm@(_, m) =
    case M.lookup k m of
    Just e' 
        |  e' == newE   -> fm
        |  otherwise    -> (True, M.insert k newE m)
        where newE = f e e'
    Nothing             -> (True, M.insert k e m)


{-|
    The 'insertChanged' this does the work of 'insert' except that
    the user is saying we have definitely changed the element we
    are adding, so do not bother to check for equality just force
    the fix map to become changed.
-}
insertChanged :: (Eq e, Ord k) => k -> e -> FixMap k e -> FixMap k e
insertChanged k e (_, m) = (True, M.insert k e m)
    
{-|
  When inserting into the fix map the list of actions, we cannot
  simply use 'union', why? Because the union of two lists
  which both contain the same elements is not necessarily equal to
  either list. What we want is a function which checks if we are
  adding to the list.
  This function must first check if the first list given contains
  any items that the second list does not have, if so then then
  we are updating the 'FixMap' otherwise we are not.
-}
insertList :: (Eq e, Ord k) => 
              k -> [e] -> FixMap k [e] -> FixMap k [e]
insertList k newElems fm@(_, m) =
    case M.lookup k m of
    Just currentElems
        | any (\x -> not $ elem x currentElems) newElems ->
          (True, M.insert k (union newElems currentElems) m)
        | otherwise                             -> fm
    Nothing             -> (True, M.insert k newElems m)


{-|
  Returns element to which the given key is mapped to in the given 'FixMap'.
-}
lookup :: Ord k => k -> FixMap k a -> Maybe a
lookup k (_, mapping) = M.lookup k mapping

{-|
  Return the mapping as a list of pairs.
-}
toList :: FixMap k a -> [(k, a)]
toList = M.toList . toMap

{-|
  Return the mapping at the heart of a FixMap, useful for returning a more general
  map after the iteration has completed.
-}
toMap :: FixMap k a -> M.Map k a
toMap = snd

{-|
  And now we come to the main purpose of this module.
  This function takes in a function which perhaps modifies a fix map, but also
  perhaps returns the one given. This function repeatedly calls the given function
  on the fixmap until it is unchanged.
  Note, clearly the user must be careful to provide a function which will eventually
  reach a fixed point.
-}

fixPoint :: (FixMap k a -> FixMap k a) -> FixMap k a -> FixMap k a
fixPoint f inputMap
    | modified  = fixPoint f (False, outputMap)
    | otherwise = inputMap
    where (modified, outputMap)  = f inputMap
