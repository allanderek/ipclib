{-|
   This is a very simple model which augments the main ipc command-line
   options with those specific to smc
-}
module Smc.Cli
   ( smcVersion
   , SmcOptions   ( .. )
   , smcOptions
   , smcGraphOptions
   , srmcManual
   )
where

{- Standard library modules imported -}
import System.Console.GetOpt
   ( OptDescr   ( .. )
   , ArgDescr   ( .. )
   )
{- External Library modules imported -}
{- Local modules imported -}
import Ipc.Cli
   ( CliOpt         ( .. )
   , Manual
   , ipcManual
   , optDescsOfManual
   )
{- End of module imports -}

smcVersion :: String
smcVersion = "This is smc version 0.01"

{-| The kind of options available in the smc compiler /in addition/ to those
   available to generic ipc commands.
-}
data SmcOptions =
   PepaCondorDir FilePath
 | IpcArgs String
 | IpcLocation FilePath
 | NoGraphTitles
 | IntersperseCdfsPdfs
 | JustGenerate
 | StartExperiment Int
 | ExperimentLimit Int
   deriving Eq

smcOptions :: [ OptDescr ( CliOpt SmcOptions ) ]
smcOptions = 
  optDescsOfManual srmcManual

smcGraphOptions :: [ OptDescr ( CliOpt SmcOptions ) ]
smcGraphOptions =
  optDescsOfManual srmcGraphOnly

srmcManual :: Manual SmcOptions
srmcManual = ipcManual ++ srmcOnlyManual ++ srmcGraphOnly


srmcOnlyManual :: Manual SmcOptions
srmcOnlyManual =
   [ (header, entries) ]
   where
   header = unlines [ "\\section{Smc specific options}"
                    , "The options in this section apply only to the"
                    , "\\srmc compiler and are not related to equivalent"
                    , "options in the \\ipc\\ compiler."
                    ]
   entries = 
    [ ( unlines [ "The \\srmc\\ compiler will generate many files"
                , "By default these will be placed in the same directory"
                , "as that which contains the source \\texttt{.srmc} file."
                , "The option \\ipcflag{output-dir}"
                , "allows the user to specify a separate directory"
                , "into which to place all of the generated files."
                , "For backwards compatibility the option"
                , "\\ipcflag{pepa-condor-dir} is a synonym for"
                , "\\ipcflag{output-dir}"
                ]
      , Option ""     [ "output-dir", "pepa-condor-dir" ]
        (ReqArg (CliNonStandard . PepaCondorDir) "Dir")
        "The directory in which to store all the produced pepa models"
      )

    , ( unlines [ "By default the \\srmc compiler will assume that"
                , "the \\ipc\\ compiler is installed in a globally visible"
                , "location (on Unix systems in the user's PATH)."
                , "If \\ipc\\ is installed elsewhere the user may"
                , "point directly to the location with the"
                , "\\ipcflag{ipc} option."
                ]
      , Option ""     [ "ipc" ]
        (ReqArg (CliNonStandard . IpcLocation) "PATH")
        "A possibly non-standard location of the ipc executable to run"
      )

    , ( unlines [ "The \\srmc\\ compiler generates batch files to"
                , "invoke the \\ipc\\ compiler over the generated"
                , "\\pepa\\ files. Should the user wish to pass"
                , "additional flags to the \\ipc\\ compiler the"
                , "\\ipcflag{ipc-args} flag allows for this."
                ]
      , Option ""     [ "ipc-args" ]
        (ReqArg (CliNonStandard . IpcArgs) "argument string")
        ( "A string containing arguments which will be passed on to ipc"
          ++ " when for example outputting a batch script to solve all the"
          ++ " generated pepa models" )
      )
    , ( unlines [ "The \\srmc\\ compiler generates PEPA models and"
                , "a performance tree file and then then uses \\ipc"
                , "to run the performance tree thereby evaluating"
                , "the generated PEPA models."
                , "Sometimes we just wish to generate the models."
                , "The \\ipcflag{just-generate} flag will prevent"
                , "smc from invoking the \\ipc compiler"
                ]
      , Option ""     [ "just-generate" ]
        (NoArg (CliNonStandard JustGenerate))
        ( "Just generate the PEPA models do not evaluate them" )
      )
    , ( unlines [ "By default the smc compiler will attempt all the experiments"
                , "it's useful when parallelising the computation to spread"
                , "to tell the compiler to skip some at the start and only do"
                , "a prescribed amount."
                , "This options tells \\smc\\ to start not from experiment 0"
                , "but from the given number."
                ]
      , Option ""     [ "start-experiment" ]
        (ReqArg (CliNonStandard . StartExperiment . read) "INT")
        "Start from the given experiment number"
      )
    , ( unlines [ "By default the smc compiler will attempt all the experiments"
                , "it's useful when parallelising the computation to spread"
                , "to tell the compiler to skip some at the start and only do"
                , "a prescribed amount."
                , "This options tells \\smc\\ to perform at most the given"
                , "number of experiments"
                ]
      , Option ""     [ "experiment-limit" ]
        (ReqArg (CliNonStandard . ExperimentLimit . read) "INT")
        "Perform at most the given number of experiments"
      )
    ]


srmcGraphOnly :: Manual SmcOptions
srmcGraphOnly =
   [ (header, entries) ]
   where
   header = unlines [ "\\section{Smc Graph Options}"
                    , "The options in this section are for smcgraph"
                    ]
   entries = 
    [ ( unlines [ "The smcgraph tool will generate graphs with titles"
                , "too suppress the titles use the"
                , "\\ipcflag{no-graph-titles} flag"
                ]
      , Option ""     [ "no-graph-titles" ]
        (NoArg (CliNonStandard  NoGraphTitles))
        "Do not generate titles for the graphs"
      )
    , ( unlines [ "Normally the smcgraph tool will output, for"
                , "for each sensitivity group all of the cdfs"
                , "followed by all of the pdfs"
                , "This options supresses that behaviour to allow"
                , "the interspersing of the cdfs and pdfs"
                ]
      , Option ""     [ "intersperse-sensitivity-graphs" ]
        (NoArg (CliNonStandard IntersperseCdfsPdfs))
        "Intersperse the cdf and pdf sensitivity graphs"
      )
    ]
