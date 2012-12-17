{-|
  A data type for representing names, such that we can mangle names
  or give unique names but still hold the original representation which
  is generally useful for error reporting or matching up with separate
  user input.
-}
module Language.Pepa.QualifiedName
   ( ShowOrig             ( .. )
   , QualifiedName        ( .. )
   , textual
   , equalTextual
   , hprintQualifiedName
   , pprintQualifiedName
   , unqualified
   , getOrigName
   , sameOrigName
   , qualifyName
   , qualifyQName
   , suffixQName
   , prefixQName
   , concatQNames
   , qualifiersOfQName
   , isPathPrefix
   , localName
   ) 
   where
{- Imported Standard Libraries -}
import Data.Char
  ( isUpper
  , toLower
  )
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Text.PrettyPrint.HughesPJ as Pretty
{- Imported External Library Modules -}
{- Imported Local Libraries -}
{- End of Imports -}

{-|
  The 'ShowOrig' type class is similar to the @Show@ class.
  However it is used where we want to give a message to the user.
  Often a parsed piece of pepa syntax will be manipulated in someway
  to allow it to be more managable in the rest of the compiler.
  If we want to print some information about it to the user (say an
  error message involving it) then showing the mangled version will
  not be very helpful. So we want to show the entity as it was originally
  seen by the user. Of course for a type to implement this class
  properly then it must be able to 'get back' the original form, this
  will most often be done simply by carrying the original form
  around with it.
-}
class ShowOrig a where
    showOrig :: a -> String


{-|
  A qualified name is an identifier together with a distinguishing
  integer. We also allow there to be no qualifying number.
  It used to be that we could just represent these with the simple
  identifier, to qualify the identifier @ident@ we just did
  @ ident ++ (show i) @, where @i@ was the qualifying integer.
  However we now wish to be able to check qualified names against
  their original name, and also I think this way is better since we
  know whether or not we have already qualified the name.

  We also allow then mangling of names, this generally involves
  the addition of a prefix or suffix.
  Currently we cannot mangle and qualify a qualified name.
-}
data QualifiedName = 
    Qualified   String [ Int ]
  | Mangled     String String
  | Unqualified String
  | NameSpaceId String QualifiedName
  deriving (Eq, Ord, Show, Read)


{-|
  Show the name as it was originally parsed, this is most useful for
  giving output to the user.
-}
instance ShowOrig QualifiedName where
    showOrig (Qualified ident _)           = ident
    showOrig (Mangled _ ident)             = ident
    showOrig (Unqualified ident)           = ident
    showOrig (NameSpaceId namespace qname) = concat [ namespace
                                                    , "::"
                                                    , showOrig qname
                                                    ]

{-| Returns the canonical textual representation of a qualified name.
    This should be the same as 'hprintQualifiedName' but it's possible
    that at some later date we will change 'hprintQualifiedName' to
    some other implementation
-}
textual :: QualifiedName -> String
textual = hprintQualifiedName

{-| Returns true if two qualified names have the same textual representation -}
equalTextual :: QualifiedName -> QualifiedName -> Bool
equalTextual = Function.on (==) textual

{-| Human print a qualified name -}
hprintQualifiedName :: QualifiedName -> String
hprintQualifiedName (Qualified ident il)   = 
  List.intercalate "_" (ident : map show il)
hprintQualifiedName (Mangled prefix ident) = 
  concat [ prefix, "_", ident ]
hprintQualifiedName (Unqualified ident)    = 
  ident
hprintQualifiedName (NameSpaceId ident qn) = 
  concat [ prefix, "__", qnShown ]
  where
  prefix
    | isUpperId qnShown = ident
    | otherwise         = uncapitaliseFirst ident
  qnShown = hprintQualifiedName qn

  isUpperId :: String -> Bool
  isUpperId ""      = False
  isUpperId (c : _) = isUpper c

  uncapitaliseFirst :: String -> String
  uncapitaliseFirst []         = []
  uncapitaliseFirst (h : rest) = (toLower h) : rest

{-| An unusual pprint function in that we usually define the
    pprint function and then the hprint function uses that, but
    here it is the otherway around.
-}
pprintQualifiedName :: QualifiedName -> Pretty.Doc
pprintQualifiedName = Pretty.text . hprintQualifiedName

{-| Create a name with no qualifications.
-}
unqualified :: String -> QualifiedName
unqualified = Unqualified

getOrigName :: QualifiedName -> String
getOrigName = showOrig

sameOrigName :: QualifiedName -> QualifiedName -> Bool
sameOrigName = Function.on (==) showOrig


{-|
  Takes a simple string identifier and qualifies it with the given integer.
-}
qualifyName :: String -> Int -> QualifiedName
qualifyName c i 
    | i < 0     = error "qualifyName: negative argument"
    | otherwise = Qualified c [i]


{-| Further qualify an already qualified name.
    Note that the qualified name may not actually be qualifed,
    that is it may have been created with 'Unqualified'
-}
qualifyQName :: QualifiedName -> Int -> QualifiedName 
qualifyQName (Unqualified ident ) n   = Qualified ident [n]
qualifyQName (Qualified ident il) n   = Qualified ident $ il ++ [n]
qualifyQName (NameSpaceId ident qn) n = NameSpaceId ident $ qualifyQName qn n
qualifyQName name@(Mangled _ _)     _ =
    error $ "qualifyModel: Attempt to qualify a mangled name: " ++ (show name)


{-|
  Add a suffix to a qualified name, useful for obscuring the name in the
  output so that it won't clash with other names, for example appending
  "PEPA" to names.
-}
suffixQName :: QualifiedName -> String -> QualifiedName
suffixQName (Qualified ident i)    suffix = Qualified (ident ++ suffix) i
suffixQName (Mangled prefix ident) suffix = Mangled prefix (ident ++ suffix)
suffixQName (Unqualified ident)    suffix = Unqualified (ident ++ suffix)
suffixQName (NameSpaceId ident qn) suffix = 
   NameSpaceId ident $ suffixQName qn suffix

{-|
  The same as 'suffixQName' adds a prefix instead.
-}
prefixQName :: QualifiedName -> String -> QualifiedName
prefixQName (Qualified ident i)    prefix = Qualified   (prefix ++ ident) i
prefixQName (Mangled p ident)      prefix = Mangled p   (prefix ++ ident)
prefixQName (Unqualified ident)    prefix = Unqualified (prefix ++ ident)
prefixQName (NameSpaceId ident qn) prefix = 
   NameSpaceId ident $ prefixQName qn prefix


{-|
   Concatenates two qualified names putting the separator in
   between. 
-}
concatQNames :: QualifiedName -> String -> QualifiedName -> QualifiedName
concatQNames (Qualified s1 i1) sep (Qualified s2 i2) =
   Qualified (s1 ++ sep ++ s2) (i1 ++ i2)
concatQNames (Unqualified s1) sep (Unqualified s2)   =
   Unqualified $ s1 ++ sep ++ s2
concatQNames (Mangled s1 m1) sep (Mangled s2 m2)
   | m1 == m2  = Mangled (s1 ++ sep ++ s2) m1
   | otherwise = Mangled (s1 ++ sep ++ s2) (m1 ++ m2)
concatQNames (NameSpaceId _ _ ) _ _                  =
   error "attempt to concatenate namespaced names"
concatQNames _ _ (NameSpaceId _ _ )                  =
   error "attempt to concatenate namespaced names"
-- If we are concatenating two different 'kinds' of qualified name
-- then the result is unlikely to dependent on what kind we make
-- so we just combine the original names and make in unqualified
-- this is probably not correct behaviour, but ah well it will do
-- for now.
concatQNames s1 sep s2
   | qualifiers == [] = Unqualified combined
   | otherwise        = Qualified combined qualifiers
   where
   combined = (showOrig s1) ++ sep ++ (showOrig s2)
   qualifiers = (qualifiersOfQName s1) ++ (qualifiersOfQName s2)

qualifiersOfQName :: QualifiedName -> [ Int ]
qualifiersOfQName (Qualified _ is)   = is
qualifiersOfQName (Mangled _ _)      = []
qualifiersOfQName (Unqualified _)    = []
qualifiersOfQName (NameSpaceId _ qn) = qualifiersOfQName qn


{-| returns true if the first name is a path prefix of the second
-}
isPathPrefix :: QualifiedName -> QualifiedName -> Bool
isPathPrefix (NameSpaceId path1 rest1) (NameSpaceId path2 rest2) =
  (path1 == path2) && (isPathPrefix rest1 rest2)
isPathPrefix name1 (NameSpaceId path2 _rest2)                    =
  (textual name1) == path2
isPathPrefix _ _                                                 =
  False

{-| Local name, returns the local part of a name space id.
    So localName Main::Disk::Working = Working
-}
localName :: QualifiedName -> QualifiedName
localName (NameSpaceId _ qn)    = localName qn
localName l@(Qualified _ _is)   = l
localName l@(Mangled _ _)       = l
localName l@(Unqualified _)     = l
