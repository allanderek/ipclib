{-
-}
module Main
  ( main )
where

{- Standard Library Modules Imported -}
import Control.Arrow
  ( second )
import Control.Monad
  ( unless )
import Data.List
  ( intercalate
  , find
  )
import System.Cmd
  ( system )
import System.Console.GetOpt
  ( getOpt
  , usageInfo
  , ArgOrder    ( .. )
  , OptDescr    ( .. )
  , ArgDescr    ( .. )
  )
import System.Directory
  ( doesDirectoryExist )
import System.Environment
  ( getArgs )
import System.FilePath
  ( combine )
{- External Library Modules Imported -}
{- Local Modules Imported -}
{- End of Imports -}

data CliFlag =
    CliHelp
  | CliVersion
  | CliSingleRates
  deriving Eq

          
options :: [ OptDescr CliFlag ]
options =
  [ Option   "h"     [ "help" ]
    (NoArg CliHelp)
    "Print the help message to standard out and then exit"

  , Option   "v"     [ "version" ]
    (NoArg CliVersion)
    "Print out the version of this program"
      
  , Option   ""      [ "single-rates" ]
    (NoArg CliSingleRates)
    "Reduce all rate arrays to length one"
  ]

helpMessage :: String
helpMessage =
  usageInfo "prog-name" options

versionMessage :: String
versionMessage = "This is version 0.001"

-- | The main exported function 
main :: IO ()
main = getArgs >>= processOptions

processOptions :: [ String ] -> IO ()
processOptions cliArgs =
  case getOpt Permute  options cliArgs of
    (flags, args, [])       -> 
      processArgs flags args
    (_flags, _args, errors) -> 
      ioError $ userError (concat errors ++ helpMessage)


processArgs :: [ CliFlag ] -> [ String ] -> IO ()
processArgs flags files
  | elem CliHelp flags    = putStrLn helpMessage
  | elem CliVersion flags = putStrLn versionMessage
  | null files            = mapM_ (processModel flags) uportalsModels
  | otherwise             = do putStrLn "no arguments are accepted"
                               putStrLn helpMessage

processModel :: [ CliFlag ] -> (String, DynamicModel) -> IO ()
processModel flags (modName, dynModel) =
  do dirExists <- doesDirectoryExist name
     unless (not dirExists) $
          (system $ "mkdir " ++ name) >> return ()
     writeFile pepaFile pepaModelContent
     writeFile srmcFile srmcModelContent
     _exitCode <- system $ "./run_dir.sh " ++ name
     return ()
  where
  name             | elem CliSingleRates flags = modName ++ "__sr"
                   | otherwise                 = modName

  pepaFile         = combine name "pepa.srmc"
  srmcFile         = combine name "srmc.srmc"
  pepaModelContent = displayModel flags $ createPepaModel dynModel
  srmcModelContent = displayModel flags $ createSrmcModel dynModel


data  DynamicModel = 
  DynamicModel [ RateDef ] [ ProcessDef ] DynamicComponent

type RateDef = (String, [ String ])

data ProcessDef = 
    SequentialDef [ (String, DynamicComponent) ]
  | SetDef        String [ String ]

data DynamicComponent =
    IdProcess String
  | Prefix String DynamicComponent
  | ComponentSum DynamicComponent DynamicComponent
  | Cooperation DynamicComponent String DynamicComponent


displayModel :: [ CliFlag ] -> DynamicModel -> String
displayModel flags (DynamicModel rDefs defs system) =
  unlines $ concat [ rDefLines
                   , defLines
                   , [ "// System Equation", displayComponent system ]
                   ]
  where
  rDefLines    = map displayRateDef rDefs
  reduceArrays = elem CliSingleRates flags
                   
  displayRateDef :: RateDef -> String
  displayRateDef (name, [])        = error $ "Invalid rate def: " ++ name
  displayRateDef (name, [ value ]) = unwords [ name, "=", value, ";" ]
  displayRateDef (name, values )
    | reduceArrays                 = unwords [ name, "=", head values, ";" ]
    | otherwise                    = unwords [ name
                                             , "="
                                             , "{"
                                             , intercalate "," values
                                             ,  "} ;" 
                                             ]

  defLines = map displayDef defs

  displayDef :: ProcessDef -> String
  displayDef (SequentialDef sdefs) =
    unlines $ map displaySdef sdefs
    where
    displaySdef :: (String, DynamicComponent) -> String
    displaySdef (name, proc) =
      unwords [ name
              , "="
              , displayComponent proc
              , ";" 
              ]
  displayDef (SetDef name names)   =
    unwords [ name
            , "="
            , "{"
            , intercalate "," names
            , "} ;"
            ] 

displayComponent :: DynamicComponent -> String
displayComponent (IdProcess name)              = name
displayComponent (Prefix trans next)           = 
  trans ++ "." ++ (displayComponent next)
displayComponent (ComponentSum left right)     =
  unwords [ displayComponent left
          , "+"
          , displayComponent right
          ]
displayComponent (Cooperation left coop right) =
  unwords [ "("
          , displayComponent left
          , coop
          , displayComponent right
          , ")"
          ]


-- The uportals model
uportalsModels :: [ (String, DynamicModel) ]
uportalsModels = 
  [ ("H__E__E", h__e__e)
  , ("H__EL__E", h__el__e)
  , ("H__EL__EL", h__el__el)
  , ("H__ELB__EL", h__elb__el)
  , ("H__ELB__ELB", h__elb__elb)
  , ("H__ELBP__ELB", h__elbp__elb)
  , ("H__ELBP__ELBP", h__elbp__elbp)
  , ("HS__ELBP__ELBP", hs__elbp__elbp)
  ]
  where
  -- This is common to all the models.
  system   = Cooperation client "<upload, download, supload, sdownload>"
             (Cooperation upload "||" download)

  client   = IdProcess "Client"
  upload   = IdProcess "Upload"
  download = IdProcess "Download"

  -- Client   = Harry
  -- Upload   = Edinburgh
  -- Download = Edinburgh            
  h__e__e  = DynamicModel (concat [ edinRates, harryRates ])
                          [ edinUploadDef
                          , edinDownloadDef
                          , SetDef "Upload" [ "Edin_Upload_Idle" ]
                          , SetDef "Download" [ "Edin_Download_Idle" ]
                          
                          , harryDef
                          , SetDef "Client" [ "Harry_Idle" ]
                          ]
                          system

  -- Client   = Harry
  -- Upload   = Edinburgh, LMU
  -- Download = Edinburgh            
  h__el__e = DynamicModel (concat [ edinRates, lmuRates, harryRates ])
                          [ edinUploadDef
                          , edinDownloadDef
                          , lmuUploadDef
                          
                          , SetDef "Upload" [ "Edin_Upload_Idle"
                                            , "Lmu_Upload_Idle"
                                            ]
                          , SetDef "Download" [ "Edin_Download_Idle" ]

                          , harryDef
                          , SetDef "Client" [ "Harry_Idle" ]
                          ]
                          system

  -- Client   = Harry
  -- Upload   = Edinburgh, LMU
  -- Download = Edinburgh, LMU            
  h__el__el = 
    DynamicModel (concat [ edinRates, lmuRates, harryRates ])
                 [ edinUploadDef
                 , edinDownloadDef
                 , lmuUploadDef
                 , lmuDownloadDef
                 
                 , SetDef "Upload" [ "Edin_Upload_Idle"
                                   , "Lmu_Upload_Idle"
                                   ]
                 , SetDef "Download" [ "Edin_Download_Idle"
                                     , "Lmu_Download_Idle"
                                     ]

                 , harryDef
                 , SetDef "Client" [ "Harry_Idle" ]
                 ]
                 system

  -- Client   = Harry
  -- Upload   = Edinburgh, LMU, Bolonga
  -- Download = Edinburgh, LMU            
  h__elb__el = 
    DynamicModel (concat [ edinRates, lmuRates, bolRates, harryRates ])
                 [ edinUploadDef
                 , edinDownloadDef
                 , lmuUploadDef
                 , lmuDownloadDef
                 , bolUploadDef
                 
                 , SetDef "Upload" [ "Edin_Upload_Idle"
                                   , "Lmu_Upload_Idle"
                                   , "Bol_Upload_Idle"
                                   ]
                 , SetDef "Download" [ "Edin_Download_Idle"
                                     , "Lmu_Download_Idle"
                                     ]

                 , harryDef
                 , SetDef "Client" [ "Harry_Idle" ]
                 ]
                 system

  -- Client   = Harry
  -- Upload   = Edinburgh, LMU, Bolonga
  -- Download = Edinburgh, LMU, Bolonga
  h__elb__elb = 
    DynamicModel (concat [ edinRates, lmuRates, bolRates, harryRates ])
                 [ edinUploadDef
                 , edinDownloadDef
                 , lmuUploadDef
                 , lmuDownloadDef
                 , bolUploadDef
                 , bolDownloadDef
                 
                 , SetDef "Upload" [ "Edin_Upload_Idle"
                                   , "Lmu_Upload_Idle"
                                   , "Bol_Upload_Idle"
                                   ]
                 , SetDef "Download" [ "Edin_Download_Idle"
                                     , "Lmu_Download_Idle"
                                     , "Bol_Download_Idle"
                                     ]

                 , harryDef
                 , SetDef "Client" [ "Harry_Idle" ]
                 ]
                 system

  -- Client   = Harry
  -- Upload   = Edinburgh, LMU, Bolonga, Pisa
  -- Download = Edinburgh, LMU, Bolonga, Pisa
  h__elbp__elb = 
    DynamicModel (concat [ edinRates, lmuRates, bolRates, piRates, harryRates ])
                 [ edinUploadDef
                 , edinDownloadDef
                 , lmuUploadDef
                 , lmuDownloadDef
                 , bolUploadDef
                 , bolDownloadDef
                 , piUploadDef
                 
                 , SetDef "Upload" [ "Edin_Upload_Idle"
                                   , "Lmu_Upload_Idle"
                                   , "Bol_Upload_Idle"
                                   , "Pi_Upload_Idle"
                                   ]
                 , SetDef "Download" [ "Edin_Download_Idle"
                                     , "Lmu_Download_Idle"
                                     , "Bol_Download_Idle"
                                     ]

                 , harryDef
                 , SetDef "Client" [ "Harry_Idle" ]
                 ]
                 system

  -- Client   = Harry
  -- Upload   = Edinburgh, LMU, Bolonga, Pisa
  -- Download = Edinburgh, LMU, Bolonga, Pisa
  h__elbp__elbp = 
    DynamicModel (concat [ edinRates, lmuRates, bolRates, piRates, harryRates ])
                 [ edinUploadDef
                 , edinDownloadDef
                 , lmuUploadDef
                 , lmuDownloadDef
                 , bolUploadDef
                 , bolDownloadDef
                 , piUploadDef
                 , piDownloadDef
                 
                 , SetDef "Upload" [ "Edin_Upload_Idle"
                                   , "Lmu_Upload_Idle"
                                   , "Bol_Upload_Idle"
                                   , "Pi_Upload_Idle"
                                   ]
                 , SetDef "Download" [ "Edin_Download_Idle"
                                     , "Lmu_Download_Idle"
                                     , "Bol_Download_Idle"
                                     , "Pi_Download_Idle"
                                     ]

                 , harryDef
                 , SetDef "Client" [ "Harry_Idle" ]
                 ]
                 system

  -- Client   = Harry
  -- Upload   = Edinburgh, LMU, Bolonga, Pisa
  -- Download = Edinburgh, LMU, Bolonga, Pisa
  hs__elbp__elbp = 
    DynamicModel (concat [ edinRates, lmuRates, bolRates
                         , piRates, harryRates, sallyRates ])
                 [ edinUploadDef
                 , edinDownloadDef
                 , lmuUploadDef
                 , lmuDownloadDef
                 , bolUploadDef
                 , bolDownloadDef
                 , piUploadDef
                 , piDownloadDef
                 
                 , SetDef "Upload" [ "Edin_Upload_Idle"
                                   , "Lmu_Upload_Idle"
                                   , "Bol_Upload_Idle"
                                   , "Pi_Upload_Idle"
                                   ]
                 , SetDef "Download" [ "Edin_Download_Idle"
                                     , "Lmu_Download_Idle"
                                     , "Bol_Download_Idle"
                                     , "Pi_Download_Idle"
                                     ]

                 , harryDef
                 , sallyDef
                 , SetDef "Client" [ "Harry_Idle"
                                   , "Sally_Idle"
                                   ]
                 ]
                 system


  --- Edinburgh upload
  edinUploadDef     = SequentialDef [ ("Edin_Upload_Idle", edinUploadIdle)
                                    , ("Edin_Upload_Broken", edinUploadBroken)
                                    ]
  edinUploadIdle     = ComponentSum edinUploadUpload edinUploadFail
  edinUploadUpload   = Prefix "(upload, edin_avail * edin_lambda)"
                              (IdProcess "Edin_Upload_Idle")
  edinUploadFail     = Prefix "(fail, edin_mu)" (IdProcess "Edin_Upload_Broken")
  edinUploadBroken   = Prefix "(repair, edin_gamma)" (IdProcess "Edin_Upload_Idle")

  --- Edinburgh download
  edinDownloadDef    = SequentialDef [ ("Edin_Download_Idle", edinDownloadIdle)
                                       , ("Edin_Download_Broken", edinDownloadBroken)
                                       ]
  edinDownloadIdle   = ComponentSum edinDownDownload edinDownloadFail
  edinDownDownload   = Prefix "(download, edin_avail * edin_delta)"
                                (IdProcess "Edin_Download_Idle")
  edinDownloadFail   = Prefix "(fail, edin_mu)" (IdProcess "Edin_Download_Broken")
  edinDownloadBroken = Prefix "(repair, edin_gamma)" (IdProcess "Edin_Download_Idle")


  -- LMU upload
  lmuUploadDef       = SequentialDef [ ("Lmu_Upload_Idle", lmuUploadIdle) ]
  lmuUploadIdle      = Prefix "(upload, lmu_avail * lmu_lambda)" $ 
                       IdProcess "Lmu_Upload_Idle"

  -- LMU download
  lmuDownloadDef     = SequentialDef [ ("Lmu_Download_Idle", lmuDownloadIdle) ]
  lmuDownloadIdle    = Prefix "(download, lmu_avail * lmu_delta)" $
                       IdProcess "Lmu_Download_Idle"


  -- Bolonga Upload
  bolUploadDef       = SequentialDef [ ("Bol_Upload_Idle", bolUploadIdle)
                                     , ("Bol_Upload_Broken", bolUploadBroken)
                                     ]
  bolUploadIdle      = ComponentSum bolUploadUp $ ComponentSum bolUploadSup
                                                               bolUploadFail
  bolUploadUp        = Prefix "(upload, bol_avail * bol_lambda)" $
                       IdProcess "Bol_Upload_Idle"
  bolUploadSup       = Prefix "(upload, bol_avail * bol_slambda)" $
                       IdProcess "Bol_Upload_Idle"
  bolUploadFail      = Prefix "(fail, bol_mu)" $ IdProcess "Bol_Upload_Broken"
  bolUploadBroken    = Prefix "(repair, bol_gamma)" $ IdProcess "Bol_Upload_Idle"
  
  -- Bolonga Download
  bolDownloadDef     = SequentialDef [ ("Bol_Download_Idle", bolDownloadIdle)
                                     , ("Bol_Download_Broken", bolDownloadBroken)
                                     ]
  bolDownloadIdle    = ComponentSum bolDownloadDown $ 
                       ComponentSum bolDownloadSdown
                                    bolDownloadFail
  bolDownloadDown    = Prefix "(download, bol_avail * bol_delta)" $ 
                       IdProcess "Bol_Download_Idle"
  bolDownloadSdown   = Prefix "(sdownload, bol_avail * bol_sdelta)" $
                       IdProcess "Bol_Download_Idle"
  bolDownloadFail    = Prefix "(fail, bol_mu)" $ IdProcess "Bol_Download_Broken"
  bolDownloadBroken  = Prefix "(repair, bol_gamma)" $ IdProcess "Bol_Download_Idle"

  -- Pisa Upload
  piUploadDef       = SequentialDef [ ("Pi_Upload_Idle", piUploadIdle)
                                     , ("Pi_Upload_Broken", piUploadBroken)
                                     ]
  piUploadIdle      = ComponentSum piUploadUp $ ComponentSum piUploadSup
                                                               piUploadFail
  piUploadUp        = Prefix "(upload, pi_avail * pi_lambda)" $
                       IdProcess "Pi_Upload_Idle"
  piUploadSup       = Prefix "(upload, pi_avail * pi_slambda)" $
                       IdProcess "Pi_Upload_Idle"
  piUploadFail      = Prefix "(fail, pi_mu)" $ IdProcess "Pi_Upload_Broken"
  piUploadBroken    = Prefix "(repair, pi_gamma)" $ IdProcess "Pi_Upload_Idle"
  
  -- Pisa Download
  piDownloadDef     = SequentialDef [ ("Pi_Download_Idle", piDownloadIdle)
                                    , ("Pi_Download_Broken", piDownloadBroken)
                                    ]
  piDownloadIdle    = ComponentSum piDownloadDown $ 
                      ComponentSum piDownloadSdown
                                   piDownloadFail
  piDownloadDown    = Prefix "(download, pi_avail * pi_delta)" $ 
                      IdProcess "Pi_Download_Idle"
  piDownloadSdown   = Prefix "(sdownload, pi_avail * pi_sdelta)" $
                      IdProcess "Pi_Download_Idle"
  piDownloadFail    = Prefix "(fail, pi_mu)" $ IdProcess "Pi_Download_Broken"
  piDownloadBroken  = Prefix "(repair, pi_gamma)" $ IdProcess "Pi_Download_Idle"


  -- the Harry Client
  harryDef          = SequentialDef [ ("Harry_Idle", harryIdle)
                                    , ("Harry_Download", harryDownload)
                                    , ("Harry_Upload", harryUpload)                             
                                    , ("Harry_Disconnect", harryDisconnect)
                                    ]
  harryIdle         = Prefix "(start, harry_upload_rate)" (IdProcess "Harry_Download")
  harryDownload     = Prefix "(download, _)" $
                      Prefix "(download, _)" $
                      Prefix "(download, _)" $ IdProcess "Harry_Upload"
  harryUpload       = Prefix "(upload, _)"   $
                      Prefix "(upload, _)"   $ IdProcess "Harry_Disconnect"
  harryDisconnect   = Prefix "(finish, harry_disconnect_rate)" $
                      IdProcess "Harry_Idle"

  -- The Sally Client
  sallyDef          = SequentialDef [ ("Sally_Idle", sallyIdle)
                                    , ("Sally_Download", sallyDownload)
                                    , ("Sally_Download1", sallyDownload1)
                                    , ("Sally_Download2", sallyDownload2)
                                    , ("Sally_Upload", sallyUpload)
                                    , ("Sally_Upload1", sallyUpload1)
                                    , ("Sally_Disconnect", sallyDisconnect)
                                    ]
  sallyIdle         = Prefix "(start, sally_upload_rate)" $ IdProcess "Sally_Download"
  sallyDownload     = ComponentSum ( Prefix "(download, _)" $ IdProcess "Sally_Download1")
                                   ( Prefix "(sdownload, _)" $ IdProcess "Sally_Download1")
  sallyDownload1    = ComponentSum ( Prefix "(download, _)" $ IdProcess "Sally_Download2")
                                   ( Prefix "(sdownload, _)" $ IdProcess "Sally_Download2")
  sallyDownload2    = ComponentSum ( Prefix "(download, _)" $ IdProcess "Sally_Upload")
                                   ( Prefix "(sdownload, _)" $ IdProcess "Sally_Upload")

  sallyUpload       = ComponentSum ( Prefix "(upload, _)" $ IdProcess "Sally_Upload1" )
                                   ( Prefix "(supload, _)" $ IdProcess "Sally_Upload1" )
  sallyUpload1      = ComponentSum ( Prefix "(upload, _)" $ IdProcess "Sally_Disconnect" )
                                   ( Prefix "(supload, _)" $ IdProcess "Sally_Disconnect" )

  sallyDisconnect   = Prefix "(finish, sally_disconnect_rate)" $
                      IdProcess "Sally_Idle"


  -- Rate definitions
  edinRates = [ ("edin_lambda", [ "1.65" ] )
              , ("edin_mu",     [ "0.0275" ] )
              , ("edin_gamma",  [ "0.125" ] )
              , ("edin_delta",  [ "3.215" ] )
              , ("edin_avail",  [ "0.6", "0.7", "0.8", "0.9", "1.0" ] )
              ]
  lmuRates  = [ ("lmu_lambda", [ "0.965" ] )
              , ("lmu_delta",  [ "2.576" ] )
              , ("lmu_avail",  [ "0.5", "0.6", "0.7", "0.8", "0.9" ] )
              ]
  bolRates  = [ ("bol_lambda"  , [ "1.65" ] )
              , ("bol_mu"      , [ "0.0275" ] )
              , ("bol_gamma"   , [ "0.125" ] )
              , ("bol_delta"   , [ "3.215" ] )
              , ("bol_slambda" , [ "1.25" ] )
              , ("bol_sdelta"  , [ "2.255"] )
              , ("bol_avail"   , [ "0.8", "0.9", "1.0" ] )
              ]
  piRates   = [ ("pi_lambda"  , [ "1.65" ] )
              , ("pi_mu"      , [ "0.0275" ] )
              , ("pi_gamma"   , [ "0.125" ] )
              , ("pi_delta"   , [ "3.215" ] )
              , ("pi_slambda" , [ "1.25"  ] )
              , ("pi_sdelta"  , [ "2.255" ] )
              , ("pi_avail"   , [ "0.8", "0.9", "1.0" ] )
              ]

  harryRates = [ ("harry_upload_rate"    , [ "1.0" ] )
               , ("harry_disconnect_rate", [ "1.0" ] )
               ]
  sallyRates = [ ("sally_upload_rate"    , [ "1.0" ] )
               , ("sally_disconnect_rate", [ "1.0" ] )
               ]








-- Translation of a dynamic model into an srmc model
-- basically we need not do any translation
createSrmcModel :: DynamicModel -> DynamicModel
createSrmcModel = id

-- Translation of a dynamic model into a (large) pepa model.
-- Here for each process set we must translate into a single
-- component which may act as any of the processes in that set.
createPepaModel :: DynamicModel -> DynamicModel
createPepaModel (DynamicModel rDefs defs system) =
  DynamicModel rDefs newDefs system
  where
  newDefs = traverseSystem [] system

  traverseSystem :: [ ProcessDef ] -> DynamicComponent -> [ ProcessDef ]
  traverseSystem soFar (IdProcess name)              =
    getDefs id soFar name
  traverseSystem soFar (Cooperation left coop right) =
    traverseSystem (traverseSystem soFar left) right
  traverseSystem _ _                                 =
    error "invalid component in system definition"


  getDefs :: (String -> String) -> [ ProcessDef ] -> String -> [ ProcessDef ]
  getDefs rename soFar name 
    | any (definesName name) soFar = soFar
    | otherwise                    =
      case find (definesName name) defs of
        Nothing                 -> error $ "Undefined process: " ++ name
        Just (SequentialDef ds) -> (SequentialDef $ map renameSDef ds) : soFar
        Just (SetDef _ ds)      -> 
          setDef : (foldl (getDefs newRename) soFar ds)
          where
          setDef = SequentialDef [ (name, foldr1 ComponentSum tauPrefixes) ]

          tauPrefixes = map ((Prefix "tau") . IdProcess) ds

          newRename :: String -> String
          newRename n
            | elem n ds = name
            | otherwise = rename n
      where
      renameSDef :: (String, DynamicComponent) -> (String, DynamicComponent)
      renameSDef = second $ renameComponent rename

  definesName :: String -> ProcessDef -> Bool
  definesName n (SequentialDef ds) = elem n $ map fst ds
  definesName n (SetDef n2 _ds)    = (n == n2)


  renameComponent :: (String -> String) -> DynamicComponent -> DynamicComponent
  renameComponent rename (IdProcess ident)         = 
    IdProcess $ rename ident
  renameComponent rename (Prefix trans next)       = 
    Prefix trans $ renameComponent rename next
  renameComponent rename (ComponentSum left right) =
    ComponentSum (renameComponent rename left)
                 (renameComponent rename right)

