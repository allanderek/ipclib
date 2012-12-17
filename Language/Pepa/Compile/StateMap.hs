{-|
  This module is intended to be a fast lookup of the states we
  have already explored.
-}
module Language.Pepa.Compile.StateMap
  ( StateConcentrations
  , StateConsMap
  , initialStateConsMap
  , stateElem
  , stateInsert
  , stateRemove
  , stateMapList
  )
where

{- Standard Library Modules Imported -}
import qualified Data.Map as Map
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.Pepa.Syntax
  ( ParsedComponentId )

{- End of Module Imports -}

{-| 
  The simplest thing would be to just have a list of parsed component id
  vs concentration (ie in ints).
-}
type StateConcentrations = [ (ParsedComponentId, Int) ]

type StateConsMap = Map.Map StateConcentrations

initialStateConsMap :: StateConcentrations -> a -> StateConsMap a
initialStateConsMap _ _ = Map.empty

stateElem :: StateConcentrations -> StateConsMap a -> Bool
stateElem = Map.member

stateInsert :: StateConcentrations -> a -> StateConsMap a -> StateConsMap a
stateInsert = Map.insert

stateRemove :: StateConcentrations -> StateConsMap a -> StateConsMap a
stateRemove = Map.delete

stateMapList :: StateConsMap a -> [ a ]
stateMapList = Map.elems


{-
{-| The type of a mapping from state concentrations to some values
    often a full state description.
-}
data StateConsMap a =
    Node ParsedComponentId (Map.Map Int (StateConsMap a))
  | Leaf a


{-|
  From an initial state specified by the initial state's concentration
  make a new state concentration map
-}
initialStateConsMap :: StateConcentrations -> a -> StateConsMap a
initialStateConsMap []               _ = error "initialStateConsMap: "
initialStateConsMap ((hname, _) : _) _ =
  Node hname Map.empty

{-|
  Make a node of a state concentration map from a partial list of
  state concentrations. Note that this should not be exported because
  it will create only part of a concentration map. We assume that the
  state concentrations are given in the correct order.
-}
makeNode :: StateConcentrations -> a -> StateConsMap a
makeNode [] a                     = Leaf a
makeNode ((hname, hcon) : rest) a =
  Node hname $ Map.singleton hcon restMap
  where
  restMap = makeNode rest a


stateElem :: StateConcentrations -> StateConsMap a -> Bool
stateElem [] (Leaf _)                             =
  True
stateElem [] _                                    = 
  error "stateElem: Impossible state concentrations"
stateElem _ (Leaf _)                              = 
  error "stateElem: Impossible state map"
stateElem ((hname, hcon) : rest) (Node name kids)
  | hname /= name = error $ unwords [ "stateElem: Impossible out of order"
                                    , "concentrations:"
                                    , show hname
                                    , "/="
                                    , show name
                                    ]
  | otherwise     = 
    case Map.lookup hcon kids of
      Just m  -> stateElem rest m
      Nothing -> False


{-|
  Inserts a state concentration into a state concentration map.
  Note that you should check if the state already exists in the mapping
  first (with 'stateElem') as this function raises an error if the
  given state concentrations are already in the mapping.
-}
stateInsert :: StateConcentrations -> a -> StateConsMap a -> StateConsMap a
stateInsert [] _ _                                    = 
  error "stateInsert: Impossible state concentrations"
stateInsert _ _ (Leaf _)                              =
  error "stateInsert: Impossible state map"
stateInsert ((hname, hcon) : rest) a (Node name kids)
  | hname /= name = error "stateInsert: Impossible out of order concentrations"
  | otherwise     = 
    Node name $ Map.alter alterF hcon kids
    where
    -- alterF :: Maybe (StateConsMap a) -> Maybe (StateConsMap a)
    alterF Nothing         = Just $ makeNode rest a
    alterF (Just m)        = Just $ stateInsert rest a m


{-|
  Removes a state from a state concentration map
-}
stateRemove :: StateConcentrations -> StateConsMap a -> StateConsMap a
stateRemove [] _       = 
  error "stateRemove: Impossible situation"
stateRemove _ (Leaf _) =
  error "stateRemove: Leaf node observed too early"
stateRemove ((hname, hcon) : rest) (Node name kids)
  | hname /= name = error "stateRemove: Inconsistent state of state map"
  | otherwise = 
    Node name $ Map.alter alterF hcon kids
    where
    alterF :: Maybe (StateConsMap a) -> Maybe (StateConsMap a)
    alterF Nothing         = error nonExist
    alterF (Just (Leaf _))
      -- Fine we have found the state concerned there are no more
      -- state concentrations so we are in a consistent state so
      -- just delete the state entry.
      | null rest          = Nothing
      -- Here though we are in an inconsistent state because we have
      -- found the leaf node but there is still something left in
      -- the state concentrations.
      | otherwise          = error inconsistent
    -- This case is simple, we have found a non-leaf node so we
    -- can recursively remove the state, we are inconsistent here
    -- if rest is null, but that will get caught in the first case
    -- of 'stateRemove'.
    alterF (Just m)        = Just $ stateRemove rest m

    nonExist     = "stateRemove: Attempt to remove non-existant state"
    inconsistent = "stateRemove: Inconsistent state concentration"

{-|
  Returns a list of the states within a StateMapCons
-}
stateMapList :: StateConsMap a -> [ a ]
stateMapList (Leaf a)      = [ a ]
stateMapList (Node _ kids) = 
  concat $ map stateMapList $ Map.elems kids

-}