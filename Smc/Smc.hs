{-|
    The main module of the smc compiler, which compiles srmc models to
    batches of pepa models.
-}
module Main
    ( main )
where

{- Standard library modules imported -}
import Control.Arrow
  ( first
  , second
  )
import Control.Monad.Trans
  ( liftIO )
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map
  ( Map )
import qualified System.FilePath as FilePath
import System.FilePath
  ( takeFileName
  , addExtension
  , dropExtension
  )
import qualified System.Exit as Exit
import System.Exit
  ( ExitCode    ( .. ) )
{- External Library modules imported -}
{- Local modules imported -}
import qualified Ipc.Cli as Cli
import Ipc.Cli
  ( getCliArgs
  , Cli            ( .. )
  , CliOpt         ( .. )
  , toCli
  )
import qualified Ipc.Ipc as Ipc
import Smc.Cli
  ( smcVersion
  , SmcOptions    ( .. )
  , smcOptions
  )
import Language.Pepa.Rates
  ( RateExpr      ( .. )
  , RateIdentifier
  )
import Language.Pepa.Syntax
  ( RateSpec )
import Language.Pepa.Srmc.Syntax
  ( SrmcModel )
import Language.Pepa.Srmc.Parser
  ( parseSrmcFile )
import qualified Language.Pepa.QualifiedName as Qualified
import Language.Pepa.Print
  ( hprintPepaModel
  , hprintRateExpr
  )
import qualified Language.Pepa.Utils as Utils
import Language.Pepa.Srmc.SrmcPepa
  ( TranslatedModel   ( .. )
  , NameSpace
  , RateArray
  , nullRateArray
  , headRateArray
  , tailRateArray
  , translateSrmc
  )

-- Imports for the performance tree generation
import qualified Language.Ptrees.DataBase as DataBase
import Language.Ptrees.DataBase
  ( DataBase
  , DataBaseEntry
  , DataBaseResult
  )
import qualified Language.Ptrees.Parser as PtreeParser
import qualified Language.Ptrees.Syntax as Ptrees
import Language.Ptrees.Syntax
  ( Ptree           ( .. )
  , ModelTree       ( .. )
  , VariableTree    ( .. )
  , RateSpecTree
  )
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( IOMainControl )
{- End of module imports -}


main :: IO ()
main = getCliArgs >>= ( processArgs . toCliSmc)

toCliSmc :: [ String ] -> Cli SmcOptions
toCliSmc = toCli smcVersion "smc" smcOptions

processArgs :: Cli SmcOptions -> IO ()
processArgs (CliValid options [ fileOne, fileTwo ])
  | ext1 == ".srmc" &&
    ext2 == ".ptree" = applyFiles options fileOne fileTwo
  | ext2 == ".srmc" &&
    ext1 == ".ptree" = applyFiles options fileTwo fileOne
  | otherwise       =
    do exitCode <- Ipc.exitFromMainControl [] fileOne return $
                   fail "You must provide one .srmc and one .ptree file"
       Exit.exitWith exitCode
  where
  ext1 = FilePath.takeExtension fileOne
  ext2 = FilePath.takeExtension fileTwo
  -- I could be less strict here and allow a different
  -- ordering, or perhaps even multiple srmc files / ptree files.
  -- Not obvious what to do if we have multiple of both.

processArgs (CliValid options [ fileOne ])
  -- What I really should do is check if the obvious ptree file
  -- is present, if so then use that, otherwise generate a ptree?
  | ext1 == ".srmc" = 
    applyFiles options fileOne ptreeFile
  | otherwise       =
    do exitCode <- Ipc.exitFromMainControl [] fileOne return $
                   fail "You must provide one .srmc and one .ptree file"
       Exit.exitWith exitCode
  where
  ext1      = FilePath.takeExtension fileOne
  base      = FilePath.dropExtension fileOne
  ptreeFile = FilePath.addExtension  base ".ptree"
processArgs (CliValid _options _)                      =
  do exitCode <- Ipc.exitFromMainControl [] "" return $
                 fail "You must provide one .srmc and one .ptree file"
     Exit.exitWith exitCode
processArgs (CliInfo  _ _ infoString)                  =
  putStrLn infoString
processArgs (CliError  _ _ errorString)                =
  putStrLn errorString

applyFiles :: [ CliOpt SmcOptions ] -> FilePath -> FilePath -> IO ()
applyFiles options srmcFile ptreeFile =
  do result   <- MainControl.runMainControlT $ 
                applyPtreeFile allOptions ptreeFile srmcFile
     -- again I should be able to translate some of the smc options
     
     exitCode <- Ipc.exitFromMainControl allOptions srmcFile return result
     Exit.exitWith exitCode
  where
  -- again we should be able to translate some of the smc options
  allOptions   = options ++  addedOptions
  -- Hmm, I would like to be able to call this without staunch
  addedOptions = [ ]

-- I'm not uber happy with this, it's kind of a mess of
-- IO ExitCode's and IOMainControls. I really need to sort
-- that out a bit.
applyPtreeFile :: [ CliOpt SmcOptions ]    -- ^ The command-line options
               -> FilePath                 -- ^ The ptree file
               -> FilePath                 -- ^ The srmc file
               -> IOMainControl ExitCode
applyPtreeFile options ptreeFile srmcFile =
  do ptree     <- PtreeParser.parsePtreeFile ptreeFile
     srmcModel <- parseSrmcFile srmcFile
     processSrmcModel options ptree srmcFile srmcModel
     

-- Process a parsed in srmc model
processSrmcModel :: [ CliOpt SmcOptions ] -- ^ Command-line options
                 -> Ptree                 -- ^ The ptree to apply to each model
                 -> FilePath              -- ^ The srmc file name
                 -> SrmcModel             -- ^ The srmc model
                 -> IOMainControl ExitCode
processSrmcModel options ptree fileName sModel =
  do tModels  <- MainControl.liftMC $ translateSrmc options sModel
     -- Create the (lazy) database
     database <-liftIO $ createLazyDataBase outputBaseName tModels ptree
     -- Before we write out the database we must convert it to the kind
     -- of database that the ptree grapher will understand. We could
     -- just create this kind of database in the first place and that
     -- is indeed tempting.
     let convert = DataBase.convertDataBaseTags database convertSrmcTag
     liftIO $ writeFile dataBaseFile $ show convert
     return ExitSuccess
  where
  dataBaseFile   = addExtension outputBaseName "generated.database"
  -- The srmc name is the name of the file without the directory
  -- or the extension
  -- srmcName       = takeBaseName fileName
 
  -- If the user has set the output directory in which to place all the 
  -- output pepa models then we update the template name to reflect this.
  outputBaseName
    | null cliDirectories       = dropExtension fileName
    | (h : _) <- cliDirectories = 
      dropExtension $ FilePath.replaceDirectory fileName h
  cliDirectories = [ directory |
                     (CliNonStandard (PepaCondorDir directory)) <- options
                   ]

{-
  Bit of a design decision here, I choose to write out the filed
  models to disk. The reason being is I think it would be risky to
  return the filed models without actually writing them to disk.
  The downside is, we cannot call this function WITHOUT writing the
  files to disk. Which may be useful. Indeed in the future we may decide
  to leave the models directly in the ptree file rather than write them
  out as filed models. At least this way I can choose between those
  two ideas while only changing this function. Although in the latter
  case it is needlessly in the IO monad.

createExperimentTree :: (Maybe Int, Maybe Int)
                     -> FilePath 
                     -> [ TranslatedModel ] 
                     -> Ptree 
                     -> IO (Int, Ptree)
createExperimentTree limits outputBaseName pModels ptree =
  do mapM_ saveFiledModel filedModels
     return (length parameters, experiment)
  where
  experiment    = Pexperiment "srmc" parameters ptree
  -- This is wrong, we're creating one large experiment when clearly
  -- we should be creating one experiment per model which is itself an
  -- experiment of changing rate values.
  parameters    =
    case limits of
      -- None given do all experiments
      (Nothing, Nothing) -> tmpParams
      -- Just somewhere to start
      (Just x, Nothing)  -> drop x tmpParams
      -- Just how many experiments to perform
      (Nothing, Just x)  -> take x tmpParams
      -- Both
      (Just x, Just y)   -> take y $ drop x tmpParams
  tmpParams     = concat $ map makeValueAssigns filedModels

  
  -- Note that you must of course actually write these files out to
  -- disk before you can really consider them 'filed' in the sense that
  -- the on disk version corresponds in anyway to the represented model.
  filedModels :: [ (FilePath, TranslatedModel) ]
  filedModels = map makeFiledModel numberedModels

  numberedModels :: [ (Int, TranslatedModel) ]
  numberedModels = zip [0..] pModels
  
  -- So based on the number of the pepa model create a file name
  -- for it.
  makeFiledModel :: (Int, TranslatedModel) -> (FilePath, TranslatedModel)
  makeFiledModel = first makeFileName
  makeFileName :: Int -> FilePath
  makeFileName number  = 
    addExtension outputBaseName suffix
    where
    suffix = (show number) ++ ".pepa"
-}

{-
  Create a list of value assignments which represent the model
  plus the rate assignments for a translated model. These will
  be used inside an experimentation node.

  Note, this could potentially return an experiment, but if we wish
  to have nested experiments, ie the first experiment ranges over the
  distinct pepa models while the second ranges over the rates, this may
  be achieved however we would require that the argument to the first
  experiment also has the rates and their arrays. This is because the
  nested experiment depends upon the argument chosen from the outer
  experiment.
  To be more clear, we would like to produce something like:
  Experiment model [ first, ModelFile "blah.0.pepa"
                   , second, ModelFile "blah.0.pepa"
                   ] 
                   Experiment no-servers [ n_1000, 1000
                                         , n_2000, 2000
                                         ]
                                         Mrateover [ servers = ?no-servers ]
                                                   ?model
  However we cannot do this because the rates which we range over will not
  be the same for each pepa model produced.
  So I think we do require a major re-think of how experimentation is done
  in performance trees, and in particular how we can hope to plot results
  from that.
  I think we should at least produce one experiment for each pepa model
  that we produce. But still ultimately I'd like to be able to store
  them all in a data base somehow.

makeValueAssigns :: (FilePath, TranslatedModel) -> [ (String, Ptrees.Value) ]
makeValueAssigns (file, tModel) =
  parameters
  where
  parameters    = zip paramNames valueList
  paramNames    = map ((nameString ++) . show) ([ 0 ..] :: [ Int ])

  -- Hmm, I'd quite like a separator other than '_' here, but dash isn't
  -- parsed and hence
  nameString    = List.intercalate "_" chosenNames
  chosenNames   = map (Qualified.textual . snd) $ slatedNamesChosen tModel

  valueList     = map Ptrees.Vmodel $ createModels permRates
  modelTree     = MmodelFile (takeFileName file)

  permRates = permuteDefinitionArrays origRates
  origRates = slatedRates tModel


  createModels :: [ RateArray ] -> [ ModelTree ]
  createModels rateArrays
      -- Importantly if there are no rate arrays involved
      -- in this model, then we simply return the ONE model.
      -- It's important that we do not return NO models.
      -- Note also it's *really* important that we do this check
      -- otherwise we have an infinite loop because 'any' is
      -- always false on a null list.
    | null rateArrays               = [ thisModel ]
    | any nullRateArray  rateArrays = []
    | otherwise                     = thisModel : restModels
    where
    thisModel  = Mrateover rateSpecs modelTree
    rateSpecs  = map makeRateSpec headSpecs
    -- mapping head here is safe because we checked above that
    -- none of the rate arrays will null. If we wish to allow
    -- non-equal rate array length then we'll have to do some kind
    -- of mapMaybe.
    headSpecs  = map headRateArray rateArrays
    restModels = createModels $ map tailRateArray rateArrays

    makeRateSpec :: RateSpec -> RateSpecTree
    makeRateSpec = second Vconcrete
-}

-- Saves the given translated pepa model to disk at the given filename
saveFiledModel :: (FilePath, TranslatedModel) -> IO ()
saveFiledModel (file, tModel) =
  writeFile file $ unlines [ hprintPepaModel $ slatedModel tModel ]


data SrmcTag      = 
     SrmcTag { srmcTagRates :: Map RateIdentifier RateExpr
             , srmcTagNames :: Map NameSpace NameSpace
             -- Could represent model here as well?
             }
             deriving (Show, Read)
type SrmcLazyResult = DataBaseResult Ptrees.Result
type SrmcDataBase   = DataBase SrmcTag SrmcLazyResult
type SrmcDbEntry    = DataBaseEntry SrmcTag SrmcLazyResult
{-
  Before we output the srmc database into a file it must be readable
  by the ptree database grapher. It is very tempting to just create
  this kind of database in the first place.
-}
convertSrmcTag :: SrmcTag -> Ptrees.DataBaseTag
convertSrmcTag srmcTag =
  Map.union rateMap nameMap
  where
  -- We must convert the rate mapping so that it is a mapping from
  -- strings to strings.
  rateMap = Map.mapKeys Qualified.textual $
            Map.map hprintRateExpr $ srmcTagRates srmcTag
  -- And the same for the srmc name spaces
  nameMap = Map.mapKeys Qualified.textual $
            Map.map Qualified.textual $ srmcTagNames srmcTag


type FiledModel = (FilePath, TranslatedModel)

createLazyDataBase ::FilePath 
                     -> [ TranslatedModel ] 
                     -> Ptree 
                     -> IO SrmcDataBase
createLazyDataBase outputBaseName tModels ptree =
  do mapM_ saveFiledModel filedModels
     return database
  where
  database = DataBase.createDataBase entries
  entries  = concatMap (makeDataBaseEntries ptree) filedModels

  -- Note that you must of course actually write these files out to
  -- disk before you can really consider them 'filed' in the sense that
  -- the on disk version corresponds in anyway to the represented model.
  filedModels :: [ FiledModel ]
  filedModels = map makeFiledModel numberedModels

  numberedModels :: [ (Int, TranslatedModel) ]
  numberedModels = zip [0..] tModels
  
  -- So based on the number of the pepa model create a file name
  -- for it.
  makeFiledModel :: (Int, TranslatedModel) -> (FilePath, TranslatedModel)
  makeFiledModel = first makeFileName
  makeFileName :: Int -> FilePath
  makeFileName number  = 
    addExtension outputBaseName suffix
    where
    suffix = (show number) ++ ".pepa"
  
makeDataBaseEntries :: Ptree -> FiledModel -> [ SrmcDbEntry ]
makeDataBaseEntries ptree (file, tModel) =
  createEntries permRates
  where
  {-
  parameters    = zip paramNames valueList
  paramNames    = map ((nameString ++) . show) ([ 0 ..] :: [ Int ])

  -- Hmm, I'd quite like a separator other than '_' here, but dash isn't
  -- parsed and hence
  nameString    = List.intercalate "_" chosenNames
  chosenNames   = map (Qualified.textual . snd) $ slatedNamesChosen tModel
  valueList     = map Ptrees.Vmodel $ 
  -}

  -- The name mapping portion of the srmcTag is the same across all
  -- of the database entries associated with a single translated model.
  nameMapping = Map.fromList $ slatedNamesChosen tModel
  permRates   = Utils.permuteAssociationLists origRates
  origRates   = slatedRates tModel


  createEntries :: [ RateArray ] -> [ SrmcDbEntry ]
  createEntries rateArrays
      -- Importantly if there are no rate arrays involved
      -- in this model, then we simply return the ONE model.
      -- It's important that we do not return NO models.
      -- Note also it's *really* important that we do this check
      -- otherwise we have an infinite loop because 'any' is
      -- always false on a null list.
    | null rateArrays               = [ thisEntry ]
    | any nullRateArray  rateArrays = []
    | otherwise                     = thisEntry : restEntries
    where
    thisEntry   = DataBase.createLazyEntry thisTag thisPtree
    thisPtree   = Ptrees.Plet "srmc" (Ptrees.Pmodel thisModel) ptree
    thisTag     = SrmcTag { srmcTagRates = rateMapping
                          , srmcTagNames = nameMapping
                          }
    rateMapping = Map.fromList headSpecs
    thisModel   = Mrateover rateSpecs modelTree
    modelTree   = MmodelFile (takeFileName file)
    rateSpecs   = map makeRateSpec headSpecs
    -- mapping head here is safe because we checked above that
    -- none of the rate arrays will null. If we wish to allow
    -- non-equal rate array length then we'll have to do some kind
    -- of mapMaybe.
    headSpecs   = map headRateArray rateArrays
    restEntries = createEntries $ map tailRateArray rateArrays

    makeRateSpec :: RateSpec -> RateSpecTree
    makeRateSpec = second Vconcrete

 
 