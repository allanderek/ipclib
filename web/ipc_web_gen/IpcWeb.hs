{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-
-}
module Main
  ( main
  , mkH1
  , mkH2
  , mkH3
  , mkH4
  , mkH5
  , mkH6
  )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( (>>>) )

import System.Console.GetOpt
  ( getOpt
  , usageInfo
  , ArgOrder    ( .. )
  , OptDescr    ( .. )
  , ArgDescr    ( .. )
  )
import System.Environment
  ( getArgs )
import System.Exit
  ( exitWith
  , ExitCode     ( .. )
  )
{- External Library Modules Imported -}
import Text.XML.HXT.Arrow
  ( runX
  , ArrowXml
  , XmlTree
  , IOSArrow
  
  , mkelem
  , sattr
  )
import qualified Text.XML.HXT.Arrow as Hxt
  
{- Local Modules Imported -}
{- End of Imports -}

data CliFlag =
    CliHelp
  | CliVersion
  deriving Eq


options :: [ OptDescr CliFlag ]
options =
  [ Option   "h"     [ "help" ]
    (NoArg CliHelp)
    "Print the help message to standard out and then exit"

  , Option   "v"     [ "version" ]
    (NoArg CliVersion)
    "Print out the version of this program"
  ]

helpMessage :: String
helpMessage =
  usageInfo "ipcwebgen" options

versionMessage :: String
versionMessage = "This is version 0.001"

-- | The main exported function
main :: IO ()
main = getArgs >>= processOptions

-- process the options using the getOpt library function.
-- If we 
processOptions :: [ String ] -> IO ()
processOptions cliArgs =
  case getOpt Permute  options cliArgs of
    (flags, args, [])       -> 
      processArgs flags args
    (_flags, _args, errors) -> 
      ioError $ userError (concat errors ++ helpMessage)

-- Currently there are no arguments so we just ignore them.
-- If we are not doing a trivial invocation then we produce the
-- web page.
processArgs :: [ CliFlag ] -> [ String ] -> IO ()
processArgs flags _files
  | elem CliHelp flags    = putStrLn helpMessage
  | elem CliVersion flags = putStrLn versionMessage
  | otherwise             = produceWebPage flags

-- Produces the web page by running the 'writeIpcPage' arrow.
produceWebPage :: [ CliFlag ] -> IO ()
produceWebPage _flags =
  do [rc]  <- runX writeIpcPage
     if rc >= Hxt.c_err
        then exitWith (ExitFailure (negate 1))
        else exitWith ExitSuccess

-- Creates an arrow in which we write the main ipc page to a file.
writeIpcPage :: IOSArrow XmlTree Int
writeIpcPage =
  Hxt.root [] [ mainIpcPage ]
  >>>
  Hxt.writeDocument [(Hxt.a_indent, Hxt.v_1)] "ipc.html"
  >>>
  Hxt.getErrStatus

-- The main ipc web page stored as an xml arrow
mainIpcPage :: ArrowXml a => a XmlTree XmlTree
mainIpcPage =
  mkelem "html" []
                [ mkelem "head" []
                                headElements
                , mkelem "body" [ sattr "class" "PEPA"
                                , sattr "bgcolor" "#FFFFFF"
                                , sattr "link"    "#FF0000"
                                , sattr "vlink"   "#FF0000"
                                , sattr "alink"   "#FF0000"
                                ]
                                ( heading :  mainIpcBody)
                                
                ]
  where
  headElements = [ mkelem "title" [] [ Hxt.txt "International PEPA Compiler" ]
                 , mkelem "meta" [ sattr "http-equiv" "Content-Type"
                                 , sattr "content" "text/html; charset=us-ascii"
                                 ] []
                 , mkelem "link" [ sattr "rel" "SHORTCUT ICON" 
                                 , sattr "href""http://www.dcs.ed.ac.uk/pepa/favicon3.ico"
                                 ] []
                 , mkelem "link" [ sattr "rel" "stylesheet" 
                                 , sattr "type" "text/css"
                                 , sattr "href" cssUrl
                                 ] []
                 , mkelem "link" [ sattr "rel" "alternate"
                                 , sattr "type" "application/rss+xml"
                                 , sattr "href" rssUrl
                                 , sattr "title" "PEPA RSS feed"
                                 ] []
                 ]
  cssUrl       = "http://www.dcs.ed.ac.uk/pepa/pepa.css"
  rssUrl       = "http://www.dcs.ed.ac.uk/pepa/news/feed.rss"


  heading      = mkCenter [ headingTable ]
  headingTable = mkTable [ sattr "align" "center" 
                         , sattr "width" "100%" 
                         , sattr "cellpadding" "8" 
                         , sattr "summary" "banner"
                         ] 
                         [ mkRow [] [ mkCell [] [ pepaLogoSmall ]
                                    , mkCell [] [ titleTable ]
                                    , mkCell [] [ pepaLogoSmall ]
                                    ]
                         ]

  titleTable     = mkTable [ sattr "align" "center" 
                           , sattr "width" "100%" 
                           , sattr "summary" perfEvalProAlg
                           ] 
                           [ titleTableRow1, titleTableRow2 ]
  perfEvalProAlg = "Performance Evaluation Process Algebra"

  titleTableRow1 = mkRow [ sattr "align" "center" ] [ mkCell [] [ pepa ] ]
  titleTableRow2 = mkRow [ sattr "align" "center" ] [ mkCell [] [ h1Title ] ]
  h1Title        = mkH1 [mkItalics [mkFontifiedText "#FF0000" "4" titleText]]
  titleText      =  "International PEPA Compiler"
  pepa           = mkFontifiedText "#FF0000" "6" "PEPA"


--------
pepaLogoSmall :: ArrowXml a => a n XmlTree
pepaLogoSmall =
  mkelem "img" attributes []
  where
  attributes = [ sattr "src" "http://www.dcs.ed.ac.uk/pepa/pepasmall.gif"
               , sattr "width" "43"
               , sattr "height" "28"
               , sattr "alt" "PEPA"
               ]


mainIpcBody :: ArrowXml a => [ a n XmlTree ]
mainIpcBody =
  [ mkParagraph [ mkBold [ mkFontifiedText colourRed "+1" theIPC ] ]
  , mkParHead "Background"
  , mkParagraph [ Hxt.txt "The International PEPA compiler ("
                , ipc
                , Hxt.txt ") is a continuation of work on an earlier compiler"
                , Hxt.txt " for PEPA, the Imperial PEPA compiler, which "
                , Hxt.txt " comiles PEPA models into the input language "
                , Hxt.txt "of Will Knottenbelt's "
                , dnamaca
                , Hxt.txt " tool."
                , Hxt.txt "Work on the Imperial PEPA Compiler was undertaken "
                , Hxt.txt "at Imperial College, London "
                , Hxt.txt "and is available from there."
                , Hxt.txt "See the "
                , mkLink "http://www.doc.ic.ac.uk/ipc/"
                  [ Hxt.txt "Imperial PEPA compiler web site" ]
                , Hxt.txt " for further details."
                ]

  , mkParHead "Modelling Features"
  , mkParagraph [ Hxt.txt "IPC supports the full PEPA language plus "
                , Hxt.txt "some non-standard extensions. "
                , Hxt.txt "The extensions are:"
                ]
  , mkUlist $ map (mkListItem . (: []) . Hxt.txt) features
  , mkParagraph [ Hxt.txt "Thorough static analysis of the input PEPA model "
                , Hxt.txt "is performed. "
                , Hxt.txt "The current list of static analysis checks detects:"
                ]
  , mkUlist $ map (mkListItem . (: []) . Hxt.txt) sanalyses

  , mkParHead "Measurement Features"
  , mkParagraph [ Hxt.txt "The International PEPA Compiler supports the full "
                , Hxt.txt "measurement specification language XSP"
                , Hxt.txt "(eXtended Stochastic Probes). "
                , Hxt.txt "This allows us to specify the set(s) of states"
                , Hxt.txt "in which we are interested in measuring. "
                , Hxt.txt "IPC natively supports the computation of "
                , Hxt.txt "probability density and cumulative distribution"
                , Hxt.txt "functions for passage-time queries. "
                , Hxt.txt "If one specifies that compilation should use the "
                , dnamaca
                , Hxt.txt " tool to perform the actual numerical analysis then"
                , Hxt.txt "one can also perform transient and steady-state "
                , Hxt.txt "analysis of the model."
                ]

  , mkParHead "Visualisation Features"
  , mkParagraph [ Hxt.txt "The model may (with or without the addition of "
                , Hxt.txt "measurement probes) may be output as a "
                , mkelem "tt" [] [ Hxt.txt ".dot" ]
                , Hxt.txt "file which is in turn converted to a scalable"
                , Hxt.txt "vector graphics file (SVG) depicting the state "
                , Hxt.txt "space of the input model. The "
                , mkelem "tt" [] [ Hxt.txt ".dot" ]
                , Hxt.txt "file may also be transformed into a PDF file "
                , Hxt.txt "or other output formats see "
                , mkLink "www.graphviz.org"
                         [ Hxt.txt "the graphviz website" ]
                , Hxt.txt " for more details."
                ]

  , mkParHead "Experimental Features"
  , mkUlist $ map (mkListItem . (: []) . Hxt.txt) expFeats

  , mkParHead "Implementation Details"
  , mkParagraph [ Hxt.txt "The International PEPA Compiler is written "
                , Hxt.txt "in the lazy functional programming language "
                , mkLink "http://www.haskell.org"
                         [ Hxt.txt "Haskell" ]
                ]

  , mkParHead "Downloading and Installing IPC"
  , mkParagraph [ Hxt.txt "We provide binary distributions of the "
                , mkBold [ Hxt.txt "smc/ipclib/hydra" ]
                , Hxt.txt " tool chain for Windows and Linux. "
                , Hxt.txt "Windows users must have "
                , mkBold [ Hxt.txt "cygwin" ]
                , Hxt.txt " installed. It is available for download at "
                , mkLink "http://www.cygwin.com" 
                         [ Hxt.txt "http://www.cygwin.com" ]
                , Hxt.txt ". Ensure that you include "
                , mkBold [ Hxt.txt "gcc"] 
                , Hxt.txt " and "
                , mkBold [ Hxt.txt "g++"] 
                , Hxt.txt " in your distribution."
                ]
  , mkUlist $ map mkListItem downInstr
  , mkOlist $ map mkListItem instInstr

  , mkParHead "Building ipc/ipclib and smc from source"
  , mkParagraph [ Hxt.txt "The source code for the ipc and smc compilers"
                , Hxt.txt " is contained within the ipclib source distribution."
                , Hxt.txt " This in turn is stored in a "
                , mkLink "http://darcs.net" [ Hxt.txt "darcs" ]
                , Hxt.txt "  repository. "
                , Hxt.txt "There are two ways to obtain the source"
                ]
  , mkOlist $ map mkListItem getSourceI
  , mkParagraph [ Hxt.txt "Once you have obtained the source code the "
                , Hxt.txt "library and related tools can be compiled "
                , Hxt.txt "with the following commands:"
                ]
  , mkOlist $ map mkListItem buildCommands
  , mkParagraph [ Hxt.txt "Optionally to install the package "
                , Hxt.txt "run the following command:"
                ]
  , installCommand

  , mkParagraph [ Hxt.txt "If you do not have root permissions "
                , Hxt.txt "(and even if so it's generally a good idea) "
                , Hxt.txt "you can install it to a local directory of "
                , Hxt.txt "your choice, such as: "
                , mkBold [ Hxt.txt "${HOME}/install" ]
                , Hxt.txt " by exchanging the first command for the command:"
                ]
  , mkCommandLine "runhaskell Setup.hs configure --user --prefix ${HOME}/install"
  , mkParagraph [ Hxt.txt "This will place the executable programs in: "
                , mkBold [ Hxt.txt "${HOME}/install/bin/" ]
                , Hxt.txt " which should be in your path."
                ]

  , mkParHead "Compiling ipclib under ghc 6.6"
  , mkParagraph [ Hxt.txt "During the switch from ghc version 6.6 to "
                , Hxt.txt "ghc version 6.8 the 'base' libraries were split "
                , Hxt.txt "into several separate packages and the new 'base' "
                , Hxt.txt "package was much smaller. For example 'Data.Map' "
                , Hxt.txt "was put into the package 'containers'. "
                , Hxt.txt "This means that the ipclib.cabal file specifies as "
                , Hxt.txt "packages which the build depends upon some which were "
                , Hxt.txt "simply not packages in ghc version 6.6. "
                , Hxt.txt "Therefore your build configuration will fail with some "
                , Hxt.txt "message about build dependencies "
                , Hxt.txt "'directory and containers' not being met."
                ]
  , mkParagraph [ Hxt.txt "To resolve this it is recommended that you upgrade "
                , Hxt.txt "to ghc version 6.8, if however this is "
                , Hxt.txt "impossible/awkward then in the darcs repository "
                , Hxt.txt "there is a cabal file which should work with ghc "
                , Hxt.txt "version 6.6. This is called: "
                , mkBold [ Hxt.txt "ghc.6.6.ipclib_cabal" ]
                , Hxt.txt ". To make your build work do, overwrite the "
                , mkBold [ Hxt.txt "ipclib.cabal" ]
                , Hxt.txt " with the "
                , mkBold [ Hxt.txt "ghc6.6.ipclib_cabal" ]
                , Hxt.txt " file. You will also need Neil Mitchell's "
                , mkBold [ Hxt.txt "filepath" ]
                , Hxt.txt " library (which is included with the ghc 6.8 "
                , Hxt.txt "version but not 6.6) you can download and install "
                , Hxt.txt "in the usual cabal way from "
                , mkLink "http://www-users.cs.york.ac.uk/~ndm/filepath/"
                         [ Hxt.txt "Neil's homepage" ]
                ]
  , mkParagraph [ Hxt.txt "If you do this and you wish to send a patch, "
                , Hxt.txt "please remember "
                , mkBold [ Hxt.txt "not" ]
                , Hxt.txt " to record the (irrelevant) changes to the "
                , mkBold [ Hxt.txt "ipclib.cabal" ]
                , Hxt.txt "file."
                ]

  , mkParHead "Hydra from Source"
  , mkParagraph [ Hxt.txt "Hydra is an optional companion to "
                , ipc
                , Hxt.txt " which may help to solve larger models. "
                , Hxt.txt "The source for "
                , hydra
                , Hxt.txt " (a Markov chain solver) can also be downloaded "
                , Hxt.txt "via darcs. The command is: "
                , mkCommandLine hydDarcsGet
                , Hxt.txt "Again the source can alternatively be downloaded "
                , Hxt.txt "via the tarball at: "
                , mkLink hydTarUrl [ Hxt.txt "hydra.tar.gz" ]
                ]
  , mkParagraph [ Hxt.txt "To compile and install issue the commands:" ]
  , mkOlist $ map mkListItem hydraCommands
  , mkParagraph [ Hxt.txt "Usually you will need to become 'root' "
                , Hxt.txt "for the final command. "
                , Hxt.txt "As for most autoconf managed programs if "
                , Hxt.txt "you wish to install in a non-standard location "
                , Hxt.txt "then provide the "
                , mkBold [ Hxt.txt "./configure" ]
                , Hxt.txt " command with a "
                , mkBold [ Hxt.txt "==prefix" ]
                , Hxt.txt " option. A typical user install is: "
                ]
   , mkOlist $ map mkListItem hydUserComms                
   , mkParagraph [ Hxt.txt "Making sure that "
                 , mkBold [ Hxt.txt "${HOME}/install/bin" ]
                 , Hxt.txt " is in your "
                 , mkBold [ Hxt.txt "$PATH" ]
                 , Hxt.txt " environment variable."
                 ]
  ]
  where
  -- mkParHead :: String -> a n XmlTree
  mkParHead s = mkParagraph [ mkBold [ mkFontifiedText colourRed "" s ] ]

  colourRed = "#FF0000"
  theIPC    = "IPC: The International PEPA Compiler"
  ipc       = mkBold [ Hxt.txt "ipc" ]
  hydra       = mkBold [ Hxt.txt "Hydra" ]
  dnamaca   = mkBold [ Hxt.txt "DNAmaca" ]

  features  = [ "immediate actions with optional immediate rates;" 
              , "functional rates; and"
              , "process arrays with or without cooperation"
              ]
  sanalyses = [ "an undefined rate parameter which is used;"
              , "a defined rate parameter which is not then used;"
              , "an undefined process name which is used"
              , "a defined process name which is not used;"
              , "self-loops on states " ++
                "(which have no meaning at the Markov chain level);"
              , "deadlocked states;"
              , "cooperations in which one or both sides do not perform "
                ++ "all of the actions in the cooperation set; and"
              , "unnecessary hiding of actions which are not performed by the component."
              ]
  expFeats  = [ "translation into PRISM model format - This " ++
                  "works well for transformation to an explicit " ++
                  "state space but not as well for PRISM's native model " ++
                  "description format."
              , "Translation into FSP - The implementation of this is " ++
                   "quite advanced but requires further testing."
              , "Translation into Dizzy format - We have only an " ++ 
                   "immature implementation so far."
              ]

  downInstr = [ [ Hxt.txt "To download the Windows binary distribution, click "
                , mkLink windistUrl [ Hxt.txt "here" ]
                ]
              , [ Hxt.txt "To download the Linux binary distribution, click "
                , mkLink lindistUrl [ Hxt.txt "here" ]
                , Hxt.txt " - It has been successfully tested on "
                , Hxt.txt "Linux Fedora Core 6"
                ]
              ]

  aclarkUrl  = "http://homepages.inf.ed.ac.uk/aclark6/downloads/"
  windistUrl = aclarkUrl ++ "ipclib-win-binary-distribution.zip"
  lindistUrl = aclarkUrl ++ "ipclib-linux-binary-distribution.tar.gz"

  instInstr = [ [ Hxt.txt "Unzip the distribution zip file "
                , Hxt.txt "into your filesystem."
                ]
              , [ Hxt.txt "Update your "
                , mkBold [ Hxt.txt "PATH" ]
                , Hxt.txt " environment variable with "
                , mkBold [ Hxt.txt "<local_dir>/ipclib/bin" ]
                , Hxt.txt " where "
                , mkBold [ Hxt.txt "<local_dir>" ]
                , Hxt.txt " is the directory into which you "
                , Hxt.txt "unzipped the distribution."
                ]
              ]

  getSourceI = [ [ Hxt.txt "If you have darcs and you wish to possibly "
                 , Hxt.txt "modify the source then this is the best way "
                 , Hxt.txt "to obtain the source code. "
                 , Hxt.txt "The repository is located at: "
                 , mkLink ipcRepoUrl [ mkBold [ Hxt.txt ipcRepoUrl ] ]
                 , Hxt.txt " You can obtain the source through "
                 , Hxt.txt " the darcs command: "
                 , mkCommandLine ipcDarcsGet
                 ]
               , [ Hxt.txt "Alternatively one can download a "
                 , mkLink ipcTarUrl [ Hxt.txt "source tarball" ]
                 , Hxt.txt "And then issue the command: "
                 , mkCommandLine "tar xzf ipclib.tar.gz"
                 ]
               ]

  ipcRepoUrl  = "http://groups.inf.ed.ac.uk/srmc/ipc/ipclib/"
  ipcTarUrl   = ipcRepoUrl ++ "ipclib.tar.gz"
  ipcDarcsGet = "darcs get " ++ ipcRepoUrl

  buildCommands = [ [ mkBold [ Hxt.txt "runhaskell Setup configure" ] ]
                  , [ mkBold [ Hxt.txt "runhaskell Setup build" ] ]
                  ]

  installCommand = mkOlist [ mkListItem 
                             [ mkBold [ Hxt.txt "runhaskell Setup install" ] ]
                           ]

  hydRepoUrl  = "http://groups.inf.ed.ac.uk/srmc/ipc/hydra"
  hydTarUrl   = hydRepoUrl ++ "hydra.tar.gz"
  hydDarcsGet = "darcs get " ++ hydRepoUrl

  hydraCommands = listTxtCommands
                  [ "cd hydra/src/"
                  , "autoreconf"
                  , "./configure"
                  , "make"
                  , "make install"
                  ]

  hydUserComms  = listTxtCommands
                  [ "cd hydra/src/"
                  , "autoreconf"
                  , "./configure ==prefix ${HOME}/install/"
                  , "make"
                  , "make install"
                  ] 

  listTxtCommands = map ( (: []) . mkBold . (: []) . Hxt.txt )
  mkCommandLine s = mkUlist [ mkListItem [ mkBold [ Hxt.txt s ] ] ]
{-
<!--
    <h3>Hydra from source</h3>
-->
-}

--------
-- Couple  of help type synonyms
-- type MakeElem a n     = ArrowXml a => [ a n XmlTree ] -> a n XmlTree
-- type MakeElemWith a n = ArrowXml a => 
--                         [ a n XmlTree ] -> [ a n XmlTree ] -> a n XmlTree

--------
mkCenter :: ArrowXml a => [ a n XmlTree ] -> a n XmlTree
mkCenter = mkelem "center" []

mkTable :: ArrowXml a => [ a n XmlTree ] -> [ a n XmlTree ] -> a n XmlTree
mkTable = mkelem "table"

mkRow :: ArrowXml a => [ a n XmlTree ] -> [ a n XmlTree ] -> a n XmlTree
mkRow = mkelem "tr"

mkCell :: ArrowXml a => [ a n XmlTree ] -> [ a n XmlTree ] -> a n XmlTree
mkCell = mkelem "td"

mkH1 :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkH1 = mkelem "h1" []

mkH2 :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkH2 = mkelem "h2" []

mkH3 :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkH3 = mkelem "h3" []

mkH4 :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkH4 = mkelem "h4" []

mkH5 :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkH5 = mkelem "h5" []

mkH6 :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkH6 = mkelem "h6" []

mkItalics :: ArrowXml a => [ a n XmlTree ] -> a n XmlTree
mkItalics = mkelem "i" []

mkBold :: ArrowXml a => [ a n XmlTree ] -> a n XmlTree
mkBold = mkelem "b" []


mkFont :: ArrowXml a => [ a n XmlTree ] -> [ a n XmlTree ] -> a n XmlTree
mkFont = mkelem "font"

mkFontifiedText :: ArrowXml a => String -> String -> String -> a n XmlTree
mkFontifiedText colour size text = 
  mkFont [ sattr "color" colour
         , sattr "size" size
         ]
         [ Hxt.txt text ]

mkParagraph :: ArrowXml a => [ a n XmlTree ] -> a n XmlTree
mkParagraph = mkelem "p" []

mkLink :: ArrowXml a => String -> [ a n XmlTree ] -> a n XmlTree
mkLink url = mkelem "a" [ sattr "href" url ]

mkUlist :: ArrowXml a => [ a n XmlTree ] -> a n XmlTree
mkUlist = mkelem "ul" []

mkOlist :: ArrowXml a => [ a n XmlTree ] -> a n XmlTree
mkOlist = mkelem "ol" []

mkListItem :: ArrowXml a => [ a n XmlTree ] -> a n XmlTree
mkListItem = mkelem "li" []