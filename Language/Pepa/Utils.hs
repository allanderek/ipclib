{-|
  This is a utilities module containing some functions that are used
  throughout ipclib.
 -}
module Language.Pepa.Utils 
  ( -- * Three tuples
    fst3
  , snd3
  , trd3
    -- * String related functions
  , mkCSlist
  , commaSeparate
  , parenthise
  , indent
  , indentLines
  , indentLine
  , uncapitaliseFirst
  , doubleQuoteString
    -- * List functions
  , safeList
  , fullGroupBy
  , dropFromEnd
  , countSatisfy
  , permuteLists
  , permuteAssociationLists
    -- * File name operations
  , switchExtension
  , dropGivenExt
  , addLeadingDotSlash
    -- * Pretty printing with the HughesPJ library
  , myShowFloat
  , pprintDouble
  , pprintMaybe
    -- * Error utilities
  , errorIf
  , maybeError
    -- * External process utilities
  , getProcessOutput
  , collectErrorCodes
  )
where


{- External Library Imports -}
{- Standard Library Imports -}
import qualified Data.Char as Char
import qualified Data.List as List
import Numeric
  ( showFFloat )
import System.Exit
  ( ExitCode       ( .. ) )
import qualified System.FilePath as File
import System.IO
  ( hGetContents )
import System.Process
  ( runInteractiveCommand
  , waitForProcess
  )
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- Local module imports -}
{- End of Imports -}

-- | access the first element of a three-tuple
fst3 :: (a, b, c) -> a
fst3 (a, _, _ ) = a

-- | access the second element of a three-tuple
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | access the third element of a three-tuple
trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

{-| Create comma separated list of a string list -}
mkCSlist :: [ String ] -> String
mkCSlist = List.intercalate ","

{-| Comma Separate is the same as mkCSlist except that we also
    have a space with each comma
-}
commaSeparate :: [ String ] -> String
commaSeparate = List.intercalate ", " 

-- | Add parentheses around a string
parenthise :: String -> String
parenthise s = concat [ "(", s, ")" ]

{-| Indent all the lines of a given string -}
indent :: String -> String
indent = unlines . indentLines . lines

{-| Indents each string -}
indentLines :: [ String ] -> [ String ]
indentLines = map indentLine

{-| Prefixes some indentation to the front of a string -}
indentLine :: String -> String
indentLine = ("    " ++)

{-| Uncapitalises the first letter of a string -}
uncapitaliseFirst :: String -> String
uncapitaliseFirst ""      = ""
uncapitaliseFirst (h : t) = (Char.toLower h : t)

{-| Surround the given string in double quotes -}
doubleQuoteString :: String -> String
doubleQuoteString s = concat [ "\"", s, "\"" ]

{-| Take a list function and turn it into a safe one which
    returns the given default value whenever the list is empty
-}
safeList :: b -> ([a] -> b) -> [a] -> b
safeList b _f [] = b
safeList _b f l  = f l

{-|
   fullGroupBy operates over the whole list at all stages, unlike
   'Data.List.groupBy'.
   Hence @groupBy (==) [1, 2, 2, 1] = [[1], [2, 2], [1]]@
   but   @fullGroupBy (==) [1, 2, 2, 1] = [[1, 1], [2, 2]]@
-}
fullGroupBy :: (a -> a -> Bool) -> [ a ] -> [ [ a ] ]
fullGroupBy _ []         = []
fullGroupBy f (h : rest) =
   (h : withH) : (fullGroupBy f notWithH)
   where
   (withH, notWithH) = List.partition (f h) rest

{-| Same as 'Data.List.dropWhile' but drops from the end of the list -}
dropFromEnd :: (a -> Bool) -> [a] -> [a]
dropFromEnd f = reverse . dropWhile f . reverse

{-|
   Count the number of elements in a list which satisfy the given predicate
-}
countSatisfy :: (a -> Bool) -> [a] -> Int
countSatisfy f = length . filter f

{-|
  Permutates a list of lists. Essentially this multiplies a list of lists
  together. We return a list of lists, all of the lists returned will be
  the same length. Each index into all of the lists will be one permutation
  of the values in the original lists. So the first element of the first
  list input list will, in the returned list of lists, share a row with all
  of the elements in each of the other lists.
  Eg, suppose we are given the lists 
  @
  [1, 2, 3]
  [4, 5 ]
  [7]
  @
  Then the returned output is
  @
  [ 1, 1, 2, 2, 3, 3 ]
  [ 4, 5, 4, 5, 4, 5 ]
  [ 7, 7, 7, 7, 7, 7 ]
  @
-}
{-
  To do this, the first two cases are obvious, permuting the empty list of
  lists, or the singleton list of one list just returns the argument.
  Others, we must essentially multiply the first list with the permuted
  rest of the lists. The @permutedRest@ permutes the rest of the lists
  this should produce a list of lists all of the same size. We then repeat
  each of these the number of times equal to the length of the first list.
  The first list itself has each element repeated the number of times equal
  to the size of the original permuted lists for the rest. Essential then
  all lists should be equal to the length of the first list multiplied by
  the length of an individual list in the recursively permuted rest. Since
  all the lists in the recursively permuted rest are of equal length it
  doesn't matter which one, we take the first.
-}
permuteLists :: [[a]] -> [[a]]
permuteLists []             = []
permuteLists [ lastList ]   = [ lastList ]
permuteLists (first : rest) = 
   (newFirst : productPermed)
   where 
   permutedRest  = permuteLists rest
   productPermed = map (replicateList lengthFirst) permutedRest
   -- We can take the head of the first, since [-rest-] cannot
   -- be empty otherwise the first case would have applied.
   sizeRest      = length $ head permutedRest
   lengthFirst   = length first
   newFirst      = replicateEachElement sizeRest first


{-
   Permute set definitions, basically performs the above 'permuteLists'
   but rather than on a set of list, ie @[ [a] ]@ on a set of definition
   lists, ie a set of mappings from somethingt to lists, that is
   @ [ (a, [b]) ] @

   Note that this does rely on the above 'permuteLists' not
   changing the order of the lists.
-}
permuteAssociationLists :: [ (a, [b]) ] -> [ (a, [b]) ]
permuteAssociationLists arrays = 
   zip names newArrays
   where
   (names, shortArrays) = unzip arrays
   newArrays            = permuteLists shortArrays

{-
  Replicates a list a number of times, so for example 
  @ replicateList 3 [1,2,3] @ would be
  @ [1,2,3, 1,2,3, 1,2,3] @. To do this we replicate the list
  @n@ number of times to get a list of lists, and then concatenate them.
-}
replicateList :: Int -> [a] -> [a]
replicateList n = concat . replicate n

{-
  Replicates each element in a list a number of times, so for example
  @replicateEachElement 3 [1,2,3] @ returns
  @ [1,1,1, 2,2,2, 3,3,3] @.
-}
replicateEachElement :: Int -> [a] -> [a]
replicateEachElement _ []             = []
replicateEachElement n (first : rest) = 
    (replicate n first) ++ (replicateEachElement n rest)

{-
  Switches a suffix, this allows us to for example easily get the mod file
  from the pepa file, that is
  @switchSuffix ".pepa" ".mod" "file.pepa"@
  will evaluate to
  @"file.mod"@
-}
switchExtension :: String -> String -> FilePath -> FilePath
switchExtension currentExt desiredExt fileName =
  File.addExtension (dropGivenExt currentExt fileName) desiredExt

{-| Removes the extension from a given file name but only if it is
    the extension expected.
-}
dropGivenExt :: String -> FilePath -> FilePath
dropGivenExt currentExt fileName 
  | extension == currentExt = name
  | otherwise               = fileName
  where
  (name, extension) = File.splitExtension fileName

{-|
   Adds in a leading @.\/@ should the path not begin with a @\/@.
   This should really be from the @filepath@ library.
   Note, I'm fairly certain this won't work on Windows.
-}
addLeadingDotSlash :: FilePath -> FilePath
addLeadingDotSlash fn@('/' : _)       = fn
addLeadingDotSlash fn@('.' : '/' : _) = fn
addLeadingDotSlash fn                 = "./" ++ fn

{-
   My version of show float basically attempts to print out the value
   using 'showFFloat' but since this will print out @0.7@ as
   @0.700000000@ (if you have the digits value set to 9) I chop off
   the spurious zeros.
-}
myShowFloat :: Double -> String
myShowFloat d = 
   dropSpuriousZeros s
   where
   s = showFFloat (Just 9) d "" 

   dropSpuriousZeros :: String -> String
   dropSpuriousZeros []            = []
   dropSpuriousZeros ('.' : rest)
      -- I make it explicitly a double because when we are producing
      -- code for languages such as Java if we output "0" or "1" rather
      -- than "0.0" and "1.0" then it automatically reads in as an int.
      -- This means for example that "1 / 1 + 1" is zero and not 0.5
      -- as we would hope.
      | all (== '0') rest          = ".0"
      | otherwise                  = '.' : (dropFromEnd (== '0') rest)
   dropSpuriousZeros (h : rest)    = h : (dropSpuriousZeros rest)


{-| Pretty printing via the HughesPJ pretty printing library 
    We have our own version of pretty printing a double because
    we may wish to restrain the number of digits shown.
-}
pprintDouble :: Double -> Doc
pprintDouble = Pretty.text . myShowFloat

{-| Print a value inside a 'Maybe' if there is a value
    there, returning the empty document otherwise.
-}
pprintMaybe ::(a ->Doc) -> Maybe a -> Doc
pprintMaybe _f Nothing = Pretty.empty
pprintMaybe f (Just a) = f a

{-| If the condition is not met then we just return the given value.
    The condition being met specifies that there is an error which we
    raise with the given string.
-}
errorIf :: Bool -> String -> a -> a
errorIf True   s _a = error s
errorIf False _s  a = a

{-| Return the value held inside a 'Maybe' value, if that value is
    actually a 'Nothing' then call error with the given error message
-}
maybeError :: String -> Maybe a -> a
maybeError _errMsg (Just a) = a
maybeError errMsg  Nothing  = error errMsg

{-|
  A small function for blindly running a process until it completes
  its output and then waiting for its exit code. 
  We return both the output (excluding stderr) plus the exit code.
  NOTE: I think this is basically not the best way to do this
  don stewart's monad for this is better and some other folk may
  have done some things.
-}
getProcessOutput :: String -> IO (String, ExitCode)
getProcessOutput command =
     -- Create the process
  do (_pIn, pOut, _pErr, handle) <- runInteractiveCommand command
     -- Wait for the process to finish and store its exit code
     exitCode <- waitForProcess handle
     -- Get the standard output.
     output   <- hGetContents pOut
     -- return both the output and the exit code.
     return (output, exitCode)


{-| Perform an IO action which returns an exit code
    (so usually involves running an external command) over a list
    of objects. Collect the error codes and return 'ExitSuccess'
    if all of the commands succeeded and 'ExitFailure' otherwise.
-}
collectErrorCodes :: (a -> IO ExitCode) -> [a] -> IO ExitCode
collectErrorCodes processFun elements =
  do errorCodes <- mapM processFun elements
     if null [ i | ExitFailure i <- errorCodes ] 
        then return ExitSuccess
        else return $ ExitFailure 1


