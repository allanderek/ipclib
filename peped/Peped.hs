{-
   This is a prototype for a new editor, one which is *not* based
   on text but on syntax.
-}

{- Standard library modules imported -}
import System.Environment 
   ( getArgs )
{-
import System.Exit
   ( exitFailure
   , exitWith
   , ExitCode      ( .. )
   )
-}
import Text.ParserCombinators.Parsec
    ( parse
    )
{- External Library Modules Imported -}
import Hiil.Hask
   ( userChoice
   , askUser
   )
import Language.Pepa.Syntax
   ( ParsedComponent )
import Language.Pepa.Print
   ( HumanPrint   ( .. ) )
import Language.Pepa.Parser
   ( pepaComponent )
{- Local modules imported -}

{- End of module imports -}


main :: IO ()
main = 
   getArgs >> getUserAction initialState initialCommands
   where
   initialCommands = [ ( 'c', CreateMainSystem ) ]
   initialState    = State { processDefs = []
                           , mainSystem  = EmptyMainSystem
                           }

getUserAction :: SystemState -> [ (Char, UserCommand) ] -> IO ()
getUserAction state charCommands =
   do print state
      userChoice "What do you wish to do?" choices mDefault help
   where
   choices  = common ++ commands
   commands = map makeCommand charCommands
   
   common   = [ ( 'q', return () ) 
              -- We already re-draw so to do that just re-call this
              -- function.
              , ( 'r', getUserAction state charCommands )
              ]

   makeCommand :: (Char, UserCommand) -> (Char, IO ())
   makeCommand (c, com) = (c, performAction state com)

   mDefault = Just 'q'
   help     = Just "Just press q to quit"


performAction :: SystemState -> UserCommand -> IO ()
performAction state CreateMainSystem  = 
   do systemString <- askUser "What is the main system equation?"
      let mainEq   = parseMainSystem systemString
          newState = state { mainSystem = mainEq }
          commands = systemState newState
      getUserAction newState commands
performAction state DefineSomeProcess =
   getUserAction newState $ systemState newState
   where
   newState = state { processDefs = [ ( pName, "(a, r).P" ) ] }
   pName = case mainSystem state of
             EmptyMainSystem -> error "empty main system"
             MainSystem _c   -> "mainSys"
   

{-
   Decide what to do based on the current state of the system.
-}
systemState :: SystemState -> [ (Char, UserCommand) ]
systemState state =
   case mainSystem state of
      EmptyMainSystem -> initialCommands
      _               -> modifyCommands
   where
   initialCommands = [ ( 'c', CreateMainSystem ) ]
   modifyCommands  = [ ( 'm', CreateMainSystem )
                     , ( 'd', DefineSomeProcess)
                     ]


{- The state of the system so far -}
data SystemState = State { processDefs :: [ (String, String) ]
                         , mainSystem  :: MainSystem
                         }

{- Possible actions to do -}
data UserCommand = CreateMainSystem
                 | DefineSomeProcess

{- A pepa system component can be either an identifier or a coopertion. -}
data MainSystem = EmptyMainSystem
                | MainSystem ParsedComponent


{- Should be a parser for the main system equation -}
parseMainSystem :: String -> MainSystem
parseMainSystem "" = EmptyMainSystem
parseMainSystem s  = 
   case parse pepaComponent "" s of
      Left _  -> EmptyMainSystem
      Right c -> MainSystem c

instance Show SystemState where
   show = printSystemState

printSystemState :: SystemState -> String
printSystemState state = 
   unlines $ (map show $ processDefs state)
             ++ [ show $ mainSystem state ]

instance Show MainSystem where
   show = printSystemEquation

printSystemEquation :: MainSystem -> String
printSystemEquation (EmptyMainSystem) = "'undefined'"
printSystemEquation (MainSystem c)    = hprint c