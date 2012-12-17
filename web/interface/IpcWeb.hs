{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
 {-
 -}
module Main
  ( main )
where

{- Standard Library Modules Imported -}
import qualified Control.Monad.Trans as Monad.Trans
import Data.Char
  ( isSpace )
import qualified Data.Maybe as Maybe
import Data.Maybe
  ( mapMaybe
  , isJust
  )
import System.FilePath
  ( combine )
import Text.XHtml
  ( (<<)
  , (!)
  , (+++)
  , Html
  , HTML    ( .. )
  , HtmlAttr

  , strAttr
  )
import qualified Text.XHtml as Xhtml
{- External Library Modules Imported -}
import Network.CGI
  ( CGI
  , CGIResult
  , getInput
  )
import qualified Network.CGI as Cgi  
{- Local Modules Imported -}
import qualified Ipc.Cli as Cli
import Ipc.Ipc
  ( CliOptions
  , StateResult
  , getStatesSpaceResult
  , getStatesSizeReport
  , getPassageTimeResult
  , getPassageEndResult
  , getSteadyResult
  )
import Ipc.DrawGraph
  ( plotSimpleGraphPNG
  , makeSimpleGraph
  , SimpleLine         ( .. )
  )
import Language.Pepa.Syntax
  ( ParsedModel )
import qualified Language.Pepa.FileParser as FileParser
import Language.Pepa.MainControl
  ( MainControl
  , closeMainControlWith 
  )
import Language.Pepa.Compile.GenMatrix
  ( SteadyState
  , showSteadyResults
  )
import Language.Pepa.Compile.Uniformise
  ( PassageResult
  , PassageEndResult
  , pdfResult
  , cdfResult
  , formatCsvPassageResultCdf 
  , formatCsvPassageResultPdf 
  )
{- End of Imports -}


{-
  The main program is pretty simple we just run the CGI action.
-}
main :: IO ()
main =  Cgi.runCGI $ Cgi.handleErrors cgiMain

{-
  To be able to produce graphs which we can then display in the output
  webpage we require that our main function, that is the one which creates
  the page be in the IO monad.
-}
cgiMain :: CGI CGIResult
cgiMain = 
  do visitInfo <- getAnalysisData
     page      <- Monad.Trans.liftIO $ createPage visitInfo
     Cgi.output $ Xhtml.renderHtml page
 

{-
  We are in the IO monad here to allow us to create files, mostly graphs,
  which can be incorporated into the results page.
  Obviously this big failure in this code so far is that we just 'error'
  on parse errors in the fields (which are turned into input options),
  so this needs a bit of a refactor.
  There are two ways to achieve this, either we just turn all the fields
  into a list of strings and call the main function of IPC which will
  interpret those strings as command-line options (and return an error
  in the 'MainControl' monad if there are parse errors).
  OR we allow this function to create an error page if there are any
  parse errors.
  Or a third option, perhaps we do both, that is call 'toCliIpc' to get
  the command-line options but then still process them on our own.
-}
createPage :: Visit -> IO Html
createPage FirstVisit                         =
  -- FirstVisit, this means the user comes without any arguments
  -- so we return the default page.
  return $ makePepaPage demoPageTitle startingInputForm
createPage (ExampleVisit formInfo)            =
  -- The user has clicked on an example
  return $ makePepaPage demoPageTitle inputForm
  where
  inputForm = generalInputForm Nothing formInfo
createPage (ReRunVisit modelString formInfo)  =
  -- The user has run an analysis and pressed the "Start Again"
  -- button. So we should set up the input form to repeat the
  -- analysis because most likely they only want to make a small
  -- modification.
  return $ makePepaPage demoPageTitle inputForm
  where
  inputForm = generalInputForm (Just modelString) formInfo
createPage (ResultVisit modelString formInfo) =
  -- Okay we are go, this is a result generating visit, this means
  -- the user has clicked on the 'Submit' button
  case Cli.toCli "0.1" "ipcweb" Cli.baseCliOptions allArgs of
    (Cli.CliValid options _)       ->
      do results <- generatePage options modelString formInfo
         return $ makePepaPage resultsPageTitle results
    -- There was an error in processing the command-line arguments
    -- since we build up the command-line arguments this probably means
    -- the user has a parse error in some field, for example a syntax-error
    -- in a probe definition
    (Cli.CliError _ _ errorString) ->
      return $ makePepaPage resultsPageTitle $ 
        generateErrorPage modelString formInfo errorString
    -- This actually should never happen since 'CliInfo' is only returned
    -- if @--version@ or @--help@ is supplied which it shouldn't be.
    (Cli.CliInfo _ _ infoString)   ->
      return $ makePepaPage resultsPageTitle $
        generateResultsPage modelString formInfo (toHtml infoString)
  where
  -- The generation of the results page depends upon the kind of analysis
  generatePage =
    case analysisKind of
      AkSteady     -> generateSteadyResultsPage
      AkPassage    -> generatePassageResultsPage
      AkPassageEnd -> generatePassageEndResultsPage
      AkStatesSize -> generateStatesResultPage
      AkTransient  -> generateTransientResultsPage
  analysisKind   = fiAnalysisKind formInfo

  -- This represents all the command-line arguments which will be interpreted
  -- by the same routines as in the ipc command-line compiler. We do this so
  -- that we have the parsing of the option arguments (such as the argument
  -- to a --probe option) without re-doing all of that work here.
  -- Instead what we do is build up a string list which would be the 
  -- the same as that returned by 'System.Environment.getArgs'
  allArgs        = concat [ defaultOpts
                          , normaliseOpts
                          , probeOptions
                          , startActOpts
                          , stopActOpts
                          , startTimeOpts
                          , stopTimeOpts
                          , timeStepOpts
                          , lineWidthOpts
                          ]

  -- The default options are things which we wish to be true for all
  -- analyses initiated from the web interface.
  defaultOpts   = [ "--limit", "1000" ]

  normaliseOpts = if fiNormalise formInfo then [] else [ "--no-normalise" ]
  probeOptions  = mkOption "--probe"      $ fiProbeDef  formInfo 
  startActOpts  = mkOption "--source"     $ fiStartActs formInfo
  stopActOpts   = mkOption "--target"     $ fiStopActs  formInfo
  startTimeOpts = mkOption "--start-time" $ fiStartTime formInfo
  stopTimeOpts  = mkOption "--stop-time"  $ fiStopTime  formInfo
  timeStepOpts  = mkOption "--time-step"  $ fiTimeStep  formInfo
  lineWidthOpts = mkOption "--line-width" $ fiLineWidth formInfo

  -- Most of the options take an argument this is a utility to turn
  -- a maybe string into an option string pair. So if, for example
  -- the probe field is non empty and filled with @probe-string@
  -- then we turn it into the list
  -- @[ "--probe", probe-string ]@
  mkOption :: String -> Maybe String -> [ String ]
  mkOption _ Nothing  = []
  mkOption s (Just a) 
    | all isSpace a = []
    | otherwise     = [ s, a ]

{-
  The 'Visit' data structure holds information obtained from the cgi form
  about the state of the user's session. Either the user is first visiting
  the form, or they have input data and now wish to obtain results. Or they
  have already obtained results and wish to return to the start, however in
  this case we can at least fill out the fields with their original query
  since most users will make a similar query the next time.
-}
data Visit = FirstVisit
           | ExampleVisit FormInfo
           | ReRunVisit   String FormInfo
           | ResultVisit  String FormInfo

-- The structure for storing the data that should be
-- given by the user, that is the data from the input form.
data FormInfo =
  FormInfo { fiProbeDef     :: Maybe String
           , fiExampleName  :: Maybe String
           , fiStartActs    :: Maybe String
           , fiStopActs     :: Maybe String
           , fiStartTime    :: Maybe String
           , fiStopTime     :: Maybe String
           , fiTimeStep     :: Maybe String
           , fiAnalysisKind :: AnalysisKind
           , fiNormalise    :: Bool
           , fiLineWidth    :: Maybe String
           }

-- This should probably be specified somewhere in the command-line options
data AnalysisKind = AkPassage
                  | AkPassageEnd
                  | AkSteady
                  | AkTransient
                  | AkStatesSize

-- The cgi action to collect the inputs from the form defined by
-- 'startingInputForm'
getAnalysisData :: CGI Visit
getAnalysisData =
  do mmodel     <- getInputString inputPepaModel
     moldmodel  <- getInputString inputOldPepaModel
     mexample   <- getInputString inputExampleName
     mprobe     <- getInputString inputProbeDefinition
     manalysis  <- getInputString inputAnalysisKind
     mnormalise <- getInputString inputNormalise
     mlineWidth <- getInputString inputLineWidth
     mstarts    <- getInputString inputStartActions
     mstops     <- getInputString inputStopActions
     mstarttime <- getInputString inputStartTime
     mstoptime  <- getInputString inputStopTime
     mtimestep  <- getInputString inputTimeStep
     let formInfo = FormInfo { fiProbeDef     = mprobe
                             , fiExampleName  = mexample
                             , fiAnalysisKind = parseMAnalysis manalysis
                             , fiStartActs    = mstarts
                             , fiStopActs     = mstops
                             , fiStartTime    = mstarttime
                             , fiStopTime     = mstoptime
                             , fiTimeStep     = mtimestep
                             , fiNormalise    = isJust mnormalise
                             , fiLineWidth    = mlineWidth
                             }
     return $ createVisit formInfo mmodel moldmodel mexample
  where
  createVisit :: FormInfo -> Maybe String 
              -> Maybe String -> Maybe String -> Visit
  createVisit _ Nothing Nothing Nothing =
    -- There is no model, no old-model and no example selected
    -- so this is a first time visit
    FirstVisit
  createVisit formInfo Nothing Nothing (Just _) =
    -- The user has clicked on an example visit
    ExampleVisit formInfo
  createVisit formInfo Nothing (Just m) _example =
    -- There is an old-model but no current input model
    -- so this is a re-run visit, we are returning to
    -- the input form but can fill in the previous
    -- values for the user to modify.
    ReRunVisit m formInfo
  createVisit formInfo (Just m) _oldmodel _example =
    -- Go go, this is a click on the form, a model to analyse
    ResultVisit m formInfo 

  -- When we click on the input form all of the fields are supplied.
  -- That is all of the fields in the form are encoded into the uri.
  -- Even those which are the empty string. This is not what we want,
  -- we want an empty field to be given as 'Nothing' so this is a utility
  -- function which reads in a form-input and changes a (Just "") into
  -- Nothing. The input is the name of the field.
  getInputString :: String -> CGI (Maybe String)
  getInputString s = do res <- Cgi.getInput s
                        case res of
                          (Just "") -> return Nothing
                          _         -> return res

  -- The analysis kind we actually parse here rather than turn into an
  -- ipc option. This is because we will do different things depending
  -- on the different kind of analysis which we are performing.
  parseMAnalysis :: Maybe String -> AnalysisKind
  parseMAnalysis = maybe AkStatesSize parseAnalysisKind


{- Parse an analysis kind -}
parseAnalysisKind :: String -> AnalysisKind
parseAnalysisKind "steady"      = AkSteady
parseAnalysisKind "passage"     = AkPassage
parseAnalysisKind "passage-end" = AkPassageEnd
parseAnalysisKind "transient"   = AkTransient
parseAnalysisKind "state-size"  = AkStatesSize
parseAnalysisKind _             = AkSteady -- hmm

{- 
  Unparse an analysis kind.
  It *must* be the case that 
  @s == (unparseAnalysisKind . parseAnalysisKind) s@
  This is actually currently untrue because of the last case in
  'parseAnalysisKind' however it is at least true for all valid
  analysis-kind strings.
-}
unparseAnalysisKind :: AnalysisKind -> String
unparseAnalysisKind AkSteady     = "steady"
unparseAnalysisKind AkPassage    = "passage"
unparseAnalysisKind AkPassageEnd = "passage-end"
unparseAnalysisKind AkTransient  = "transient"
unparseAnalysisKind AkStatesSize = "state-size"




-- All of the cgi value identifiers.
inputPepaModel :: String
inputPepaModel = "input-pepa-model"
inputOldPepaModel :: String
inputOldPepaModel = "old-pepa-model"
inputExampleName :: String
inputExampleName = "pepa-example-name"
inputProbeDefinition :: String
inputProbeDefinition = "input-probe-definition"
inputStartActions :: String
inputStartActions = "input-start-actions"
inputStopActions :: String
inputStopActions = "input-stop-actions"
inputAnalysisKind :: String
inputAnalysisKind = "analysis-kind"
inputNormalise :: String
inputNormalise = "input-normalise"
inputLineWidth :: String
inputLineWidth = "input-line-width"
inputStartTime :: String
inputStartTime = "input-start-time"
inputStopTime :: String
inputStopTime = "input-stop-time"
inputTimeStep :: String
inputTimeStep = "input-time-step"


-- Example Pepa Models
-- the type of a pepa example is the html which should explain the model
-- together with the string representation of the model.
type PepaExample = (Html, String)


-- Here we are mapping the example name to an Html part and the
-- model itself. The Html part will generally describe the model
-- and suggest some analyses which can be done over it.
examplePepaModels :: [ (String, PepaExample ) ]
examplePepaModels =
  [ ( "simple-example"
    , simplePepaExample
    )
  , ( "simple-cache-example"
    , cachePepaExample
    )
  , ( "simple-aggregate-example"
    , simpleAggregateExample
    )
  ]

-- The simple PEPA example we define out of the list so that we can
-- refer to it outside of 'Maybe' should the lookup of the example name fail
simplePepaExample :: PepaExample
simplePepaExample = 
  ( Xhtml.paragraph << ("This is a very straightforward example")
  , unlines [ "// rate specifications"
            , "ra = 1.0 ;"
            , "rb = 2.0 ;"
            , ""
            , "// Process definitions"
            , "P1 = (a, 1.0) . P2 ;"
            , "P2 = (b, 2.0) . P1 ;"
            , ""
            , "// System Equation"
            , "P1 || P1"
            ]
  )

-- The simple PEPA example we define out of the list so that we can
-- refer to it outside of 'Maybe' should the lookup of the example name fail
cachePepaExample :: PepaExample
cachePepaExample = 
  ( Xhtml.concatHtml $ map (Xhtml.paragraph <<) 
    [ unlines [ "This is an example of a network with a cache"
              , "There is a single user who can make a request"
              , "to a service provider. For each request the service"
              , "may have cached the response and can therefore reply"
              , "quickly. For some requests though the service must"
              , "query the network - such requests are expected to take"
              , "longer."
              ]
    , unlines [ "An example query on this model may be a passage-time"
              , "query between a request and response. To achieve this"
              , "select the \"passage\" analysis kind and enter"
              , "\"req\" for the start actions and \"cached,network\""
              , "for the stop actions."
              ]
    ]
  , unlines [ "r = 1.0 ;"
            , ""
            , "User = (req, r) . Wait ;"
            , "Wait = (cached, infty) . User"
            , "     + (network, infty) . User"
            , "     ;"
            , "Service = (req, infty) . InCache"
            , "        + (req, infty) . Fetch"
            , "        ;"
            , "InCache = (cached, r) . Service ;"
            , "Fetch   = (fetch, r/2) . (network, r/5) . (delay, r) . Service ;"
            , ""
            , "User <req, cached, network> Service"
            ]
  )
  

simpleAggregateExample :: PepaExample
simpleAggregateExample =
  ( Xhtml.concatHtml $ map (Xhtml.paragraph <<)
    [ unlines [ "This is an example of a PEPA model with an"
              , "array of processes. We can aggregate the states"
              , "of the array allowing the solving of this model"
              , "for larger array lengths than would be possible"
              , "without aggregation."
              ]
    , unlines [ "One can still ask about the response-time of a"
              , "single client without manually splitting up the"
              , "array of clients."
              , "By providing the probe: "
              , "\"Client::compute:start, respond:stop\""
              , "we measure the response time as observed by a"
              , "single client. This can then be used to analyse"
              , "the effect on each client's observed response time"
              , "that the addition of clients makes."
              ]
    ]
  , unlines [ "r_request = 0.5  ;"
            , "r_compute = 0.01 ;"
            , "r_respond = 1.0  ;"
            , ""
            , "Client     = (compute, r_compute) . ClientReq  ;"
            , "ClientReq  = (request, r_request) . ClientWait ;"
            , "ClientWait = (respond, infty)     . Client     ;"
            , ""
            , "Server     = (request, infty)     . ServerResp ;"
            , "ServerResp = (respond, r_respond) . Server     ;"
            , ""
            , "Server[2] <request, respond> Client[5]"
            ]
  )

-- If there are no inputs then this is the first page that the user will see.
startingInputForm :: Html
startingInputForm  = generalInputForm Nothing defaultFormInfo

-- The absolute default form info where essentially nothing is filled in
defaultFormInfo :: FormInfo
defaultFormInfo = 
  FormInfo { fiProbeDef     = Nothing
           , fiExampleName  = Just "simple-example"
           , fiStartActs    = Nothing
           , fiStopActs     = Nothing
           , fiAnalysisKind = AkSteady
           , fiNormalise    = False
           , fiStartTime    = Nothing
           , fiStopTime     = Nothing
           , fiTimeStep     = Nothing
           , fiLineWidth    = Nothing
           }

-- Generates the starting input form from a given 'FormInfo'.
-- If this is a first visit then the given FormInfo will be the default
-- starting form info, but if this is the a return caused by hitting the
-- "return to start" button after the results page then we can fill in
-- the form data for the  user to modify since their query will likely be
-- similar to the first one.
generalInputForm :: Maybe String -> FormInfo -> Html
generalInputForm mModelString formInfo =
  mkTable [] [ mkRow [] [ formCell, examplesCell ] ]
  where
  formCell          = mkCell [] [ inputForm ]
  examplesCell      = mkCell [ Xhtml.valign "top" ]
                             [ Xhtml.paragraph << examplesHtml ]

  inputForm         =  Xhtml.form << formElements
  formElements      = [ explainExample
                      , Xhtml.paragraph ("Input PEPA model: " +++ textarea)
                      , Xhtml.paragraph (Xhtml.concatHtml analysisKinds)
                      , Xhtml.paragraph normalise
                      , Xhtml.paragraph probeField
                      , Xhtml.paragraph startActionsField
                      , Xhtml.paragraph stopActionsField
                      , Xhtml.paragraph startTimeField
                      , Xhtml.paragraph stopTimeField
                      , Xhtml.paragraph timeStepField
                      , Xhtml.paragraph lineWidthField
                      , Xhtml.submit "" "Submit"
                      ]

  -- pepaModel is the final model that we decide to put into the
  -- text area. It is, in decreasing order of preference
  -- 1) The pepa model that the user has already analysed and then
  --    hit the 'Start Again' button.
  -- 2) An example model from the list if the user has selected one
  -- 3) The first-time simple example, this is displayed on the user's
  --    first visit.
  pepaModel          = snd pepaPair
  -- The explainExample is a little portion of html which will explain to
  -- the user about the selected example. If the model selected is actually
  -- the previous model then this is all it will say.
  explainExample     = fst pepaPair
  -- So the pepaPair is either a pepaExample or the previous model string together
  -- with a small portion of html explaining that it is the previous model.
  pepaPair           = case mModelString of
                        Nothing -> pepaExample
                        Just m  -> ( previousParagraph, m)
  previousParagraph = Xhtml.paragraph << "Here is your previous model"

  -- So this is the pepa example that the user selected or the simplest
  -- pepa example if the user has not selected one. If the user has not
  -- selected one then the simplest example will only be used if this is
  -- a first time visit.
  pepaExample       = Maybe.fromMaybe simplePepaExample $ 
                      do name <- fiExampleName formInfo
                         lookup name examplePepaModels

  textarea          = ( Xhtml.textarea $ toHtml pepaModel ) ! textareaAttrs
  textareaAttrs     = [ Xhtml.name inputPepaModel
                      , Xhtml.cols "60"
                      , Xhtml.rows "20"
                      ]
  probeField        = mkTextField "A probe definition: "
                                  inputProbeDefinition
                                  (fiProbeDef formInfo)
  normalise         = (toHtml "Normalise: " ) +++ normaliseCheck
  normaliseCheck    = (Xhtml.checkbox inputNormalise "normalise") 
                      ! [ Xhtml.checked ]
  startActionsField = mkTextField "Start actions(default 'start'): "
                                  inputStartActions
                                  (fiStartActs formInfo)
  stopActionsField  = mkTextField "Stop actions(default 'stop'): "
                                   inputStopActions
                                   (fiStopActs formInfo)

  startTimeField    = mkTextField "Start time (default 0.0): "
                                   inputStartTime
                                   (fiStartTime formInfo)
  stopTimeField     = mkTextField "Stop time (default 10.0): "
                                  inputStopTime
                                  (fiStopTime formInfo)
  timeStepField     = mkTextField "Time step (default 0.1): "
                                  inputTimeStep
                                  (fiTimeStep formInfo)
  lineWidthField    = mkTextField "Graph Line Width (default 1.0): "
                                  inputLineWidth
                                  (fiLineWidth formInfo)

  -- Create a text field given the label to put next to it, the form id
  -- string and a possible default/old value for the field.
  mkTextField :: String -> String -> Maybe String -> Html
  mkTextField label name Nothing      = label +++ (Xhtml.textfield name)
  mkTextField label name (Just value) = 
    label +++ ((Xhtml.textfield name) ! [ Xhtml.value value ])


  analysisKinds     = [ toHtml "Please select the kind of analysis"
                      , analysisKindRadio "steady"
                      , toHtml "steady"
                      , analysisKindRadio "passage"
                      , toHtml "passage"
                      , analysisKindRadio "passage-end"
                      , toHtml "passage-end"
                      , analysisKindRadio "state-size"
                      , toHtml "state-size"
                      ]


  -- Create a radio button for the analysis kind, we use the name of
  -- the analysis kind from the given formInfo to determine whether this
  -- should be checked or not. Exactly one should be checked.
  analysisKindRadio :: String -> Html
  analysisKindRadio s
    | s == oldAnalysisString = radio ! [ Xhtml.checked ]
    | otherwise              = radio
    where 
    radio = Xhtml.radio inputAnalysisKind s 

  -- The string of the old analysis kind, we use this to determine which
  -- field should be initially checked.
  oldAnalysisString = unparseAnalysisKind $ fiAnalysisKind formInfo

  -- The html displaying the list of examples the user may click on
  examplesHtml  = mkelem "div" [ strAttr "id" "examplescontainer" ] 
                               [ examplesUlist ]
  -- The list of examples is an unordered list.
  examplesUlist = mkelem "ul" [ strAttr "id" "exampleslist" ] exampleItems

  -- Todo: the current item should be made up like this:
  -- <li id="active">
  -- <a href="http://www.dcs.ed.ac.uk/pepa" id="current" name="current">Home</a></li>
  exampleItems  = map mkExampleLink examplePepaModels

  -- Make an example link using the name of the example to determine both the
  -- url and the string to display as the link.
  mkExampleLink :: (String, PepaExample) -> Html
  mkExampleLink (name, (_html, _model)) =
    mkLinkListItem url name
    where
    url = concat [ demoUrl
                 , "?"
                 , Cgi.formEncode values
                 ]
    values = [ (inputExampleName, name) ]


-- A generic function to generate a results page, we take in the
-- the form-info to pass back on clicking the 'start again' button.
generateResultsPage :: String -> FormInfo -> Html -> Html
generateResultsPage modelString formInfo results =
  ( Xhtml.paragraph << results ) +++ resetButton
  where
  resetButton   = mkButtonLink "Start Again" url
  url           = concat [ demoUrl
                         , "?"
                         , Cgi.formEncode values
                         ]
  -- 'values' here is an encoding of all the 'formInput' which achieved
  -- these results. This means when the user clicks 'Start Again' the form
  -- is set up how they had it before, since often the user will want to
  -- perform a similar analysis with some of the fields slightly modified
  -- for example they may re-perform the same passage-time measurement but
  -- for a longer time range.
  values         = [ ( inputOldPepaModel, modelString ) 
                   , ( inputAnalysisKind, analysisString )
                   ] ++ formValues
  analysisString = unparseAnalysisKind $ fiAnalysisKind formInfo
  -- Most of the form values are stored as a @Maybe String@ so we
  -- pair them up with their form-names and lift the maybe to the whole
  -- pair. We then use 'mapMaybe' to filter out the undefined ones.
  formValues     = mapMaybe liftSnd formInputs
  formInputs     = [ (inputProbeDefinition, fiProbeDef     formInfo)
                   , (inputStartActions,    fiStartActs    formInfo)
                   , (inputStopActions,     fiStopActs     formInfo)
                   , (inputStartTime,       fiStartTime    formInfo)
                   , (inputStopTime,        fiStopTime     formInfo)
                   , (inputTimeStep,        fiTimeStep     formInfo)
                   , (inputLineWidth,       fiLineWidth    formInfo)
                   ]

  -- Utility function to lift the 'Maybe' from the second value of a
  -- pair to the whole pair, potentially throwing away the first value.
  liftSnd :: (a, Maybe b) -> Maybe (a, b)
  liftSnd (_, Nothing) = Nothing
  liftSnd (a, Just b)  = Just (a,b)


-- A generic function to generate an error page with a 'start-again'
-- button which passes back the form info passed in.
generateErrorPage :: String -> FormInfo -> String -> Html
generateErrorPage modelString formInfo = 
  (generateResultsPage modelString formInfo ) . toHtml

-- A function all of the below page generators can use to parse in
-- the PEPA model.
parsePepaFile :: String -> MainControl ParsedModel
parsePepaFile = FileParser.mainControlParse FileParser.pepaFile

{-|
  Generate the results pages for a state-space size query.
-}
generateStatesResultPage :: CliOptions a -> String -> FormInfo -> IO Html
generateStatesResultPage options modelString formInfo =
  return $ generateResultsPage modelString formInfo results
  where
  results       = closeMainControlWith displayResults displayError spaceResult
  spaceResult   = parseResult >>= getStatesSpaceResult options
  parseResult   = parsePepaFile modelString

  displayResults :: StateResult -> Html
  displayResults = toHtml . getStatesSizeReport



{-|
  Generate the results page for a passage-time query.
-}
generatePassageResultsPage :: CliOptions a -> String -> FormInfo -> IO Html
generatePassageResultsPage [] modelString formInfo      =
  return errorPage
  where
  errorPage   = generateErrorPage modelString formInfo errorString
  errorString = unlines [ "You have asked for a passage-time analysis"
                        , "however you have not supplied a probe"
                        , "or specified start/stop actions to"
                        , "define the source and target states"
                        ]
generatePassageResultsPage options modelString formInfo =
  -- The closing of the 'MainControl' must live within the IO monad
  -- because, in the case that there are results to show we write
  -- out the graph files.
  do results <- closeMainControlWith displayResults 
                                     (return . displayError)
                                     passageResult
     -- Finally generate the results page
     return $ generateResultsPage modelString formInfo results
  where
  -- Analyse the model obtaining the passage-time results. This lives
  -- in the 'MainControl' monad because we may still cause an error
  -- at this point, for example if the set of source states is empty.
  passageResult = parseResult >>= getPassageTimeResult options

  -- Parse the PEPA model
  parseResult   = parsePepaFile modelString

  -- The width of lines in the graphs
  lineWidth     = Cli.getGraphLineWidth options

  -- Display the results of the passage-time analysis
  -- This involves writing out graph files and creating image elements
  -- to those graph files. The writing out of the graph files is the
  -- reason that this must live in the IO monad.
  displayResults :: PassageResult -> IO Html
  displayResults pt =
    do plotSimpleGraphPNG pdfGraph pdfGraphFile
       plotSimpleGraphPNG cdfGraph cdfGraphFile
       -- Generate csv files and links to those files
       fileLinksUrl <- linkPassageCsvFileList [ ("main", pt) ]
       return $ graphHtml +++ fileLinksUrl
    where
    pdfGraph      = makeSimpleGraph  pdfGraphTitle [] pdfGraphLines
    cdfGraph      = makeSimpleGraph  cdfGraphTitle [] cdfGraphLines
    graphHtml     = Xhtml.concatHtml [ pdfGraphHtml
                                     , cdfGraphHtml
                                     ]
    pdfGraphHtml  = Xhtml.paragraph << pdfImage
    pdfImage      = mkelem "img" pdfAttributes []
    pdfAttributes = [ strAttr "src" pdfGraphUrl
                    , strAttr "alt" "PDF GRAPH"
                    ]
    -- At least the directory of this should be abstracted away somewhere
    pdfGraphUrl   = generatedFilesUrl "pdf.png"
    pdfGraphFile  = generatedFilesDir "pdf.png"
    pdfGraphTitle = Just "Probability density function graph"
    pdfGraphLines = [ SimpleLine { simpleLineTitle  = Just "pdf"
                                 , simpleLinePoints = pdfResult pt
                                 , simpleLineWidth  = lineWidth
                                 }
                    ]

    cdfGraphHtml  = Xhtml.paragraph << cdfImage

    cdfImage      = mkelem "img" cdfAttributes []
    cdfAttributes = [ strAttr "src" cdfGraphUrl
                    , strAttr "alt" "PDF GRAPH"
                    ]
    -- At least the directory of this should be abstracted away somewhere
    cdfGraphUrl   = generatedFilesUrl "cdf.png"
    cdfGraphFile  = generatedFilesDir "cdf.png"
    cdfGraphTitle = Just "Cumulative distribution function graph"
    cdfGraphLines = [ SimpleLine { simpleLineTitle  = Just "cdf"
                                 , simpleLinePoints = cdfResult pt
                                 , simpleLineWidth  = lineWidth
                                 }
                    ]
{-|
  Passage-end results are still a fairly new feature of ipc in general
  let alone ipcweb. This function returns the passage-end result from
  the given model or as usual may instead display an error.
-}
generatePassageEndResultsPage :: CliOptions a -> String -> FormInfo -> IO Html
generatePassageEndResultsPage [] modelString formInfo      =
  return $ generateErrorPage modelString formInfo $ 
  unlines [ "You have asked for a passage-time analysis"
          , "however you have not supplied a probe"
          , "or specified start/stop actions to"
          , "define the source and target states"
          ]
generatePassageEndResultsPage options modelString formInfo =
  -- The closing of the 'MainControl' must live within the IO monad
  -- because, in the case that there are results to show we write
  -- out the graph files.
  do results <- closeMainControlWith displayResults 
                                     (return . displayError)
                                     passEndResult
     -- Finally generate the results page
     return $ generateResultsPage modelString formInfo results
  where
  -- Generate the passage-end results from the parsed model, done inside
  -- the 'MainControl' monad as we may still encounter an error even
  -- after the static analysis of the model.
  passEndResult = parseResult >>= getPassageEndResult options

  -- Parse the PEPA model
  parseResult   = parsePepaFile modelString

  -- The width of lines in the graphs
  lineWidth     = Cli.getGraphLineWidth options

  -- We must write out two graphs, this is why this lives in the
  -- IO monad.
  displayResults :: PassageEndResult -> IO Html
  displayResults per =
    do plotSimpleGraphPNG pdfGraph pdfGraphFile
       plotSimpleGraphPNG cdfGraph cdfGraphFile
       -- generate individual csv files and links to those files
       fileLinksHtml <- linkPassageCsvFileList per
       return $ graphHtml +++ fileLinksHtml
    where
    pdfGraph      = makeSimpleGraph pdfGraphTitle [] pdfGraphLines
    cdfGraph      = makeSimpleGraph cdfGraphTitle [] cdfGraphLines
    graphHtml     = Xhtml.concatHtml [ pdfGraphHtml
                                     , cdfGraphHtml
                                     ]
    pdfGraphHtml  = Xhtml.paragraph << pdfImage
    pdfImage      = mkelem "img" pdfAttributes []
    pdfAttributes = [ strAttr "src" pdfGraphUrl
                    , strAttr "alt" "PDF GRAPH"
                    ]

    pdfGraphUrl   = generatedFilesUrl "pdf.png"
    pdfGraphFile  = generatedFilesDir "pdf.png"
    pdfGraphTitle = Just "Probability density function graph"
    pdfGraphLines = map mkPdfLine per
 
    mkPdfLine :: (String, PassageResult) -> SimpleLine
    mkPdfLine (name, passageResult) =
      SimpleLine { simpleLineTitle  = Just ("pdf-" ++ name)
                 , simpleLinePoints = pdfResult passageResult
                 , simpleLineWidth  = lineWidth
                 }

    cdfGraphHtml  = Xhtml.paragraph << cdfImage

    cdfImage      = mkelem "img" cdfAttributes []
    cdfAttributes = [ strAttr "src" cdfGraphUrl
                    , strAttr "alt" "PDF GRAPH"
                    ]

    cdfGraphUrl   = generatedFilesUrl "cdf.png"
    cdfGraphFile  = generatedFilesDir "cdf.png"
    cdfGraphTitle = Just "Cumulative distribution function graph"
    cdfGraphLines = map makeCdfLine per

    makeCdfLine :: (String, PassageResult) -> SimpleLine
    makeCdfLine (name, passageResult) =
      SimpleLine { simpleLineTitle  = Just ("cdf-" ++ name)
                 , simpleLinePoints = cdfResult passageResult
                 , simpleLineWidth  = lineWidth
                 }
                 

{-
  Displays a set of passage time results as a list of links
  to comma-separated value files. What would be much neater
  is instead of writing out the file eagerly produce a link
  that when pressed calls this cgi script with arguments such
  that it generates the file and allows you to download it.
  Though maybe that's not much better since the data would
  have to be somehow inbedded into the return webpage such
  that the link could generate the file but still.
-}
linkPassageCsvFileList :: [(String, PassageResult)] -> IO Html
linkPassageCsvFileList results =
  do fileLinkItems <- mapM linkPassageCsvFiles results
     return $ makeListHtml $ concat fileLinkItems
  where
  makeListHtml :: [ Html ] -> Html
  makeListHtml fileLinks =
    mkParagraph [ toHtml fileDesc, mkelem "ul" [] fileLinks ]
  fileDesc = unlines [ "These are links to the individual"
                     , "comma-separated-value files which can be"
                     , "used to re-create the graphs above"
                     ]

{-
  Displays passage-time results as links to comma-separated files
  which this function also writes out. This returns a list of
  html items which are *list items* hence you must wrap the returned
  html items in a list. This allows you to link to multiple passage
  results in the same list (as is done for passage-end calculations).
-}
linkPassageCsvFiles :: (String, PassageResult) -> IO [ Html ]
linkPassageCsvFiles (name, pt) =
  do writeFile pdfFile pdfLines
     writeFile cdfFile cdfLines
     return fileLinks
     where
     fileLinks = [ mkLinkListItem pdfUrl pdfName
                 , mkLinkListItem cdfUrl cdfName
                 ]
     pdfUrl    = generatedFilesUrl pdfName
     pdfFile   = generatedFilesDir pdfName 
     pdfName   = name ++ "-pdf.csv"
     pdfLines  = formatCsvPassageResultPdf pt
     cdfUrl    = generatedFilesUrl cdfName
     cdfFile   = generatedFilesDir cdfName
     cdfName   = name ++ "-cdf.csv"
     cdfLines  = formatCsvPassageResultCdf pt


-- Given a file name create a filename for the default location
-- of generated files.
generatedFilesDir :: FilePath -> FilePath
generatedFilesDir = 
  combine "/public/homepages/aclark6/web/generated_graphs/"

-- Given a file name create a url for the default location of
-- generated files.
generatedFilesUrl :: String -> String
generatedFilesUrl name = 
  "http://homepages.inf.ed.ac.uk/aclark6/generated_graphs/" ++ name
           

{-|
  Generate the page for the steady state results.
-}
generateSteadyResultsPage :: CliOptions a -> String -> FormInfo -> IO Html
generateSteadyResultsPage options modelString formInfo =
  return $ generateResultsPage modelString formInfo results
  where
  -- Obtain from the 'MainControl' structure the steady-state results
  -- or of course the error if there were any.
  results       = closeMainControlWith displayResults displayError steadyResult

  -- Run steady-state analysis over the parsed model, this is done inside
  -- the 'MainControl' monad as it may fail.
  steadyResult  = parseResult >>= getSteadyResult options

  -- Parse the PEPA model into the 'MainControl' monad.
  parseResult   = parsePepaFile modelString

  -- The function to display the steady state results if there
  -- are any (that is if we have not made an error.
  displayResults :: SteadyState -> Html
  displayResults steadystate = 
    (Xhtml.paragraph << (Xhtml.linesToHtml reportLines) )
    where
    reportLines = lines $ showSteadyResults steadystate


{-|
  A common function to display an error from the 'MainControl' monad
  in html. This is called if the analysis fails to produce results
  for the user to view.
-}
displayError  :: String -> Html
displayError = toHtml


{-|
  Generate the web-page for transient analysis of the given model.
-}
generateTransientResultsPage :: CliOptions a -> String -> FormInfo -> IO Html
generateTransientResultsPage _options modelString formInfo =
  return $ generateResultsPage modelString formInfo results
  where
  results       = toHtml errString 
  errString     = "We are sorry transient analysis is not yet implemented"



{-|
  The base url of the demo.
-}
demoUrl :: String
demoUrl = "http://homepages.inf.ed.ac.uk/cgi/aclark6/ipcweb.cgi"


{-|
  The title of the start page of the demo.
-}
demoPageTitle :: String
demoPageTitle = "International PEPA Compiler Web Demo"

{-|
  The title of a results page
-}
resultsPageTitle :: String
resultsPageTitle = demoPageTitle ++ " - Results"

-- The main ipc web page stored as an xml arrow
makePepaPage :: String -> Html -> Html
makePepaPage titleString body =
  mkelem "html" []
                [ mkelem "head" [] headElements
                , mkelem "body" [ strAttr "class" "PEPA"
                                , strAttr "bgcolor" "#FFFFFF"
                                , strAttr "link"    "#FF0000"
                                , strAttr "vlink"   "#FF0000"
                                , strAttr "alink"   "#FF0000"
                                ]
                                [ heading, bodyAndNav ]
                                
                ]
  where
  -- The main body of the page is together with the navigation list
  -- as table with one row and two cells within that row.
  bodyAndNav   = mkTable [] [ mkRow [] [ navCell, bodyCell ] ]
  navCell      = mkCell [ Xhtml.valign "top" ] [ navigation ]
  bodyCell     = mkCell [] [ body ]

  -- The navigation list that should normally appear on the left of the
  -- page.
  navigation   = mkelem "div" [ strAttr "id" "navcontainer" ] [ navList ]
  navList      = mkelem "ul"  [ strAttr "id" "navlist" ]      navItems

  -- Todo: the current item should be made up like this:
  -- <li id="active">
  -- <a href="http://www.dcs.ed.ac.uk/pepa" id="current" name="current">Home</a></li>
  navItems     = [ pepaHomeItem ""                    "PEPA home"
                 , pepaHomeItem "news"                "PEPA news"
                 , pepaHomeItem "about"               "About PEPA"
                 , pepaHomeItem "people"              "People"
                 , pepaHomeItem "papers"              "Papers"
                 , pepaHomeItem "tools"               "Tools"
                 , pepaHomeItem "examples"            "Examples"
                 , pepaClubItem "talks.html"          "PEPA-Club"
                 ]

  -- Creates a navigation list item assuming that the base url is that
  -- of the main PEPA home page.
  pepaHomeItem :: String -> String -> Html
  pepaHomeItem url = mkLinkListItem (pepaHomeUrl ++ url)
  
  -- Creates a navigation list item assuming that the base url is that
  -- of the PEPA-club home page.
  pepaClubItem :: String -> String -> Html
  pepaClubItem url = mkLinkListItem (pepaClubUrl ++ url)

  -- The base urls for the PEPA home and PEPA club sites.
  pepaHomeUrl = "http://www.dcs.ed.ac.uk/pepa/"
  pepaClubUrl = "http://homepages.inf.ed.ac.uk/aclark6/pepaclub/"

  headElements = [ mkelem "title" [] [ toHtml titleString ]
                 , mkelem "meta" [ strAttr "http-equiv" "Content-Type"
                                 , strAttr "content" "text/html; charset=us-ascii"
                                 ] []
                 , mkelem "link" [ strAttr "rel" "SHORTCUT ICON" 
                                 , strAttr "href""http://www.dcs.ed.ac.uk/pepa/favicon3.ico"
                                 ] []
                 , mkelem "link" [ strAttr "rel" "stylesheet" 
                                 , strAttr "type" "text/css"
                                 , strAttr "href" cssUrl
                                 ] []
                 , mkelem "link" [ strAttr "rel" "alternate"
                                 , strAttr "type" "application/rss+xml"
                                 , strAttr "href" rssUrl
                                 , strAttr "title" "PEPA RSS feed"
                                 ] []
                 ]
  cssUrl       = "http://www.dcs.ed.ac.uk/pepa/pepa.css"
  rssUrl       = "http://www.dcs.ed.ac.uk/pepa/news/feed.rss"


  heading      = {-Xhtml.center-} headingTable
  headingTable = mkTable [ strAttr "align" "center" 
                         , strAttr "width" "100%" 
                         , strAttr "cellpadding" "8" 
                         , strAttr "summary" "banner"
                         ] 
                         [ mkRow [] [ mkCell [] [ pepaLogoSmall ]
                                    , mkCell [] [ titleTable ]
                                    , mkCell [] [ pepaLogoSmall ]
                                    ]
                         ]

  titleTable     = mkTable [ strAttr "align" "center" 
                           , strAttr "width" "100%" 
                           , strAttr "summary" perfEvalProAlg
                           ] 
                           [ titleTableRow1, titleTableRow2 ]
  perfEvalProAlg = "Performance Evaluation Process Algebra"

  titleTableRow1 = mkRow [ strAttr "align" "center" ] [ mkCell [] [ pepa ] ]
  titleTableRow2 = mkRow [ strAttr "align" "center" ] [ mkCell [] [ h1Title ] ]
  h1Title        = mkH1 [mkItalics [mkFontifiedText "#FF0000" "4" titleText]]
  titleText      =  "International PEPA Compiler - Web Demo"
  pepa           = mkFontifiedText "#FF0000" "6" "PEPA"


--------
-- | The small pepa logo as an html element.
pepaLogoSmall :: Html
pepaLogoSmall =
  mkelem "img" attributes []
  where
  attributes = [ strAttr "src" "http://www.dcs.ed.ac.uk/pepa/pepasmall.gif"
               , strAttr "width" "43"
               , strAttr "height" "28"
               , strAttr "alt" "PEPA"
               ]



----- Utility functions for generating the web pages
-- | 'mkelem' a convenient way to make an arbitrary tagged
-- element with a given set of attributes.
mkelem :: String -> [ HtmlAttr ] -> [ Html ] -> Html
mkelem s attrs elems = 
  ( Xhtml.tag s $ Xhtml.concatHtml elems ) ! attrs

-- | Creates a paragraph element
mkParagraph :: [ Html ] -> Html
mkParagraph = mkelem "p" []

-- | Create a table element, the rows are up to the user to
--   provide in the contents of the element.
mkTable :: [ HtmlAttr ] -> [ Html ] -> Html
mkTable = mkelem "table"

-- | Creates a row element, the cells are up to the user to
--   provide as the content of the row element.
mkRow :: [ HtmlAttr ] -> [ Html ] -> Html
mkRow = mkelem "tr"

-- | Creates a cell element
mkCell :: [ HtmlAttr ] -> [ Html ] -> Html
mkCell = mkelem "td"

-- | Creates an italics element
mkItalics :: [ Html ] -> Html
mkItalics = mkelem "i" []

-- | A convenient synonym for creating some text in  given colour
--   and font size.
mkFontifiedText :: String -> String -> String -> Html
mkFontifiedText colour size text = 
  mkFont [ strAttr "color" colour
         , strAttr "size" size
         ]
         [ toHtml text ]

-- | Creates a font element.
mkFont :: [ HtmlAttr ] -> [ Html ] -> Html
mkFont = mkelem "font"

-- | Creates a header1 element
mkH1 :: [ Html ] -> Html
mkH1 = mkelem "h1" []

-- | Creates a link with the given url as the target.
--   The contents can be any html.
mkLink :: String -> [ Html ] -> Html
mkLink url = mkelem "a" [ strAttr "href" url ]

-- | Often we wish to make a list of links, this provides an easy
--   way to make such a list item given the url and the name which
--   should be displayed as the link.
mkLinkListItem :: String -> String -> Html
mkLinkListItem url name  =
  mkelem "li" [] [ mkLink url [ toHtml name ] ]

-- | Create a button whose target is the given url.
mkButtonLink :: String -> String -> Html
mkButtonLink text url =
  mkLink url [ mkelem "button" [] [ toHtml text ] ]