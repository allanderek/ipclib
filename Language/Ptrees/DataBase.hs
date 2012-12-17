module Language.Ptrees.DataBase
  ( DataBase       ( .. )
  , DataBaseEntry  ( .. )
  , DataBaseResult ( .. )
  , createDataBase
  , createLazyEntry
  , dataBaseSize
  , queryDataBase
  , queryDataBaseFirst
  , convertDataBaseTags
  , convertDataBaseResults
  , convertWithAccum
  , retrieveResults    
  , retrieveWithKey
  , retrieveResultsM
  , retrieveWithKeyM
  )
where

{- Standard Library Modules Imported -}
import Control.Monad
  ( foldM )
import qualified Data.List as List
{- External Library Modules Imported -}
{- Local Modules Imported -}
import qualified Language.Ptrees.Syntax as Ptree
import Language.Ptrees.Syntax
  ( Ptree )
{- End of Module Imports -}

{-|
  The type of a results database, this is so far simply
  a set of database entries.
  We parameterise over the tags of each entry in the database.
  The tag is the elements which can be matched against queries.
  Such that one can gather a sub-set of results for display.
  I would certainly like to try out the possibility of having
  a multi-parameter type class such that the only tags allowed
  are ones which correspond to some class of queries.

  Additionally we also generalise over the kind of result.
  We still distinguish between those which are not yet computed
  and those which are allowing for lazy databases, but we allow
  you to compute from a lazy result whatever kind of value you
  can.

  A small point here, we have one big collection of all entries
  and the entries themselves are distinguished by either being
  a lazy thunk of non-yet-computed result or a computed result.
  We could also have two sets one of lazy thunks which,
  when evaluated are moved into the computed set. One disadvantage
  is that it may be that we could have partially evaluated results.
-}
data DataBase tag result = 
     -- I'd quite like to use a set for the data base entries.
     -- The downside is that you then have to make 'DataBaseEntry'
     -- an instance of 'Ord'. May be worth looking into, however
     -- for now I'll keep things simple.
     DataBase { dbEntries :: [ DataBaseEntry tag result ] }
     deriving (Show, Read)


createDataBase :: [ DataBaseEntry tag result ] -> DataBase tag result
createDataBase entries =
  DataBase { dbEntries = entries }

{-|
   A database entry is either a performance tree to evaluate
   in which to get a result or set of results, or those
   evaluated results. Notice that we only have one tag and
   not a list of tags, if you want more than one then your
   tag type can just be a set or list of some other type such
   as string. This allows your queries to be of the:
   all tags match, or any tags match without us having to
   differentiate between such queries here.
-}
data DataBaseEntry tag result =
     DataBaseEntry { dbeTag    :: tag
                   , dbeResult :: result
                   }
     deriving (Show, Read)

createLazyEntry :: tag -> Ptree -> DataBaseEntry tag (DataBaseResult result)
createLazyEntry tag ptree =
  DataBaseEntry { dbeTag    = tag
                , dbeResult = LazyResult ptree
                }


data DataBaseResult result =
    LazyResult      Ptree   -- ^ A lazy analysis
  | ComputedResults result  -- ^ A computed result
  deriving (Show, Read)


dataBaseSize :: DataBase tag result -> Int
dataBaseSize = length . dbEntries

queryDataBase :: DataBase tag result  -- ^ The input database
              -> (tag ->Bool)         -- ^ The filter condition
              -> DataBase tag result  -- ^ The matching entries
queryDataBase database condition =
  DataBase { dbEntries = filtered }
  where
  filtered = filter (condition . dbeTag) entries
  entries  = dbEntries database

queryDataBaseFirst :: 
     DataBase tag result                  -- ^ The input database
     -> (tag ->Bool)                      -- ^ The filter condition
     -> Maybe (DataBaseEntry tag result)  -- ^ The matching entries
queryDataBaseFirst database condition =
  List.find (condition . dbeTag) $ dbEntries database


convertDataBaseTags :: DataBase t1 r -> (t1 -> t2) -> DataBase t2 r
convertDataBaseTags database convertTag =
  DataBase { dbEntries = map convertEntry $ dbEntries database }
  where
  convertEntry entry = DataBaseEntry { dbeTag    = convertTag $ dbeTag entry
                                     , dbeResult = dbeResult entry
                                     }


convertDataBaseResults :: forall m t r1 r2 . 
                          Monad m => 
                          DataBase t r1 -> (r1 -> m r2) -> m (DataBase t r2)
convertDataBaseResults database convertResult =
  do newEntries <- mapM convertEntry $ dbEntries database
     return DataBase { dbEntries = newEntries }
  where
  convertEntry :: DataBaseEntry t r1 -> m (DataBaseEntry t r2)
  convertEntry entry = 
    do newResult <- convertResult $ dbeResult entry
       return DataBaseEntry { dbeTag    = dbeTag entry
                            , dbeResult = newResult
                            }


convertWithAccum ::forall m k t r1 r2 . 
                    Monad m => 
                    DataBase t r1 -> k -> (k -> r1 -> m (k, r2)) -> m (DataBase t r2)
convertWithAccum database initialK convertResult =
  do (_newK, newEntries) <- foldM convertEntry (initialK, []) $ 
                                  dbEntries database
     return  DataBase { dbEntries = newEntries }
  where
  convertEntry :: (k, [ DataBaseEntry t r2 ]) 
               -> DataBaseEntry t r1 
               -> m (k, [ DataBaseEntry t r2 ])
  convertEntry (k,entries) entry = 
    do (k2, newResult) <- convertResult k $ dbeResult entry
       let newEntry = DataBaseEntry { dbeTag    = dbeTag entry
                                    , dbeResult = newResult
                                    }
       return (k2, newEntry : entries)

retrieveResults :: DataBase t r1 -> (r1 ->r2) -> [ r2 ]
retrieveResults database retrieve =
  map (retrieve . dbeResult) $ dbEntries database 

retrieveWithKey :: forall t r1 r2 . DataBase t r1 -> (t -> r1 ->r2) -> [ r2 ]
retrieveWithKey database retrieve =
  map get $ dbEntries database 
  where
  get :: DataBaseEntry t r1 -> r2
  get entry = retrieve (dbeTag entry) (dbeResult entry)

retrieveResultsM :: forall m t r1 r2 .
                  Monad m =>
                  DataBase t r1 -> (r1 -> m r2) -> m [ r2 ]
retrieveResultsM database retrieve =
  mapM retrieveOne entries
  where
  entries = dbEntries database
  retrieveOne :: DataBaseEntry t r1 -> m r2
  retrieveOne = retrieve . dbeResult


retrieveWithKeyM :: forall m t r1 r2 .
                  Monad m =>
                  DataBase t r1 -> (t -> r1 -> m r2) -> m [ r2 ]
retrieveWithKeyM database retrieve =
  mapM get $ dbEntries database 
  where
  get :: DataBaseEntry t r1 -> m r2
  get entry = retrieve (dbeTag entry) (dbeResult entry)