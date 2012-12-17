{-|
  This module is a simple module to assist in writing transformations
  in which new names must be produced.
-}
module Language.Pepa.Transform.NameSupply 
    ( NameSupply
    , initialNameSupply
    , getName
    )
where

{- Standard Library Modules Imported -}
import Data.Map
  ( empty
  , insert
  , lookup
  , Map
  )
import Prelude hiding
  ( lookup )
{- External Library Modules Imported -}
{- Local Modules Imported -}
{- End of Module Imports -}



{-|
  We implement a name supply as a mapping from string to integers.
  If there is currently no mapping for a given name then that means
  it is safe to use the name without re-naming it. If there is already
  mapping from the string then it is to some number which we can prefix
  to the name and update the binding. In this way we only re-name
  names which have clashes. So we don't do any needless re-naming which
  can often be nice for reading results.
-}
type NameSupply = Map String Int

{-| Initialise a name supply -}
initialNameSupply :: NameSupply
initialNameSupply = empty

{-| Retrieve a new name from a name supply and update the name supply -}
getName :: NameSupply -> String -> (NameSupply, String)
getName nameSupply name
  | Just i <- lookup name nameSupply = ( insert name (i + 1) nameSupply
                                       , makeName name i
                                       )
  | otherwise                        = ( insert name 0 nameSupply
                                       , name
                                       )
    

--getNames :: NameSupply -> [String] -> (NameSupply, [String])
--getNames nameSupply prefixes
--    = (nameSupply + length prefixes, 
--       zipWith makeName prefixes [nameSupply..])

{- This should *not* be used outside of 'getName' or 'getNames' -}
makeName :: String -> Int -> String
makeName prefix ns = prefix ++ "_" ++ show ns
