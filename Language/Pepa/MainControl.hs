{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-|
  This module will define the main monad that we will use to pass around
  data within the imperial pepa compiler.
  The main purpose here is to define a monad that will carry around the
  logged information, warnings and errors, in this way most of the major
  functions that each module exports can refer to this monad which should
  then be able to modify with very little effect on the modules which
  use this.
-}

module Language.Pepa.MainControl
   ( MainControl
   , hasResult
   , fromMainResult
   , MainControlT
   , runMainControlT
   , IOMainControl
   , liftIO -- shouldn't really be exported
   , liftMC
   , LogInfo
   , LogEntry             ( .. )
     -- * Logging levels
   , defaultLogLevel
   , debuggingLogLevel
     -- * functions to create an add to 'MainControl' structures.
   , eitherToMainControl
   , resultError
   , valueResult
   , valueResultLogLevel
   , addLogInformation
   , resultWarning
   , resultWarningLogLevel
   , makeLogEntry
   , logResult
     -- * Exiting out of a 'MainControl'
   , closeOrError
     -- * Printing the value withint a 'MainControl'
   , showMainControlA
   , displayMainControlA
   , showMainControlAWith
   , displayMainControlAWith
   , closeMainControlWith
   )
where

{- External Library Module Imports -}
{- Standard Library Module Imports -}
import Data.Map as Map
  ( Map
  , empty
  , union
  , singleton
  , fromList
  )
-- just to avoid warnings about a name clash
import Prelude hiding
  ( log )
import Control.Monad
  ( liftM )
import Control.Monad.State.Class
  ( MonadState   ( .. ) )
import Control.Monad.Trans
  ( MonadTrans   ( .. )
  , MonadIO      ( .. )
  )
{- Local Module Imports -}
{- End of Module Imports -}

{-|
  Main control is the type which is passed around between the various
  functions of the compiler. It contains both the side information together
  with the actual result.

  [@todo@]Fatal error should also contain the logging information so that
  we can write a log on an error which might help debug the error.
  Actually it should also contain the warnings as well for the same reason.
  Functions like /mapPhase/ could then add to the logging and possibly
  the warnings the fact that you have made an attempt to get the result
  out and failed.
-}
data MainControl a = 
    FatalError IpcError LogInfo
  | MainResult a [ Warning ] LogInfo

{-|
   Returns true if the MainControl has a value to inspect or is
   ultimately a failure.
-}
hasResult :: MainControl a -> Bool
hasResult (MainResult _ _ _) = True
hasResult (FatalError _ _)   = False

{-| Returns the value of a main control if it is a result and
    calls error otherwise. Not recommended for use, unless you
    have tested with 'hasResult'.
    Essentially the same as 'Data.Maybe.fromJust'
-}
fromMainResult :: MainControl a -> a
fromMainResult (MainResult a _ _) = a
fromMainResult (FatalError s _)   = error s


{-|
  The logging information is stored as a map from a name indicating the
  kind of information stored to the log information.
  The log information consists of a string which is the actual information
  to be logged along with a number indicating the importance of the information.
  The idea is that ultimately we can provide a flag --log-level x where we output
  to the log file only those entries with a log level at x or higher.
-}
type LogInfo = Map.Map String LogEntry
data LogEntry = LogEntry { logEntryData  :: String
                         , logEntryLevel :: Int
                         }

{-| The default log entry level is 0, if you want to have a higher
    than the default then use a level greater than zero and likewise
    if you want a level less than the default (which is probably rare)
    then you want a level less than zero.
-}
defaultLogLevel :: Int
defaultLogLevel = 0

{-| The debugging level will be -1 since in general we do not want to
    log debugging information unless we specifically ask for it.
-}
debuggingLogLevel :: Int
debuggingLogLevel = -1


{-|
  For now a warning is also just a string but this I would definitely
  like to have as a data type
-}
type Warning = String

{-|
  And the same goes for errors, I would definitely like to have this
  as a data type.
-}
type IpcError = String


{-
  We now make the 'MainControl' type an instance of the monad class.
  This is purely to allow the use of the do notation to string together
  calls to functions that return the main result.
-}
instance Monad MainControl where
    (>>=)    = bindControl
    return a = MainResult a [] Map.empty
    fail     = resultError

bindControl :: MainControl a -> (a -> MainControl b) -> MainControl b
bindControl (FatalError err logs) _     = FatalError err logs
bindControl (MainResult a warns logs) f =
    case f a of
    (FatalError err logs')       -> FatalError err $ Map.union logs logs'
    (MainResult b warns' logs')  -> MainResult b (warns ++ warns') 
                                                 (Map.union logs logs')


{- 
   We make a monad transformer version of our MainControl type
   this is most often useful when using MainControl within the IO monad
-}
newtype MainControlT m a = 
  MainControlT { runMainControlT :: m (MainControl a) }

bindMCT :: (Monad m) => 
           MainControlT m a -> (a -> MainControlT m b) -> MainControlT m b
bindMCT x f = 
  MainControlT $ do unwrapped <- runMainControlT x
                    case unwrapped of
                      (FatalError err logs) -> return $ FatalError err logs
                      (MainResult b _ _)    ->
                          do result <-  runMainControlT $ f b
                             return (unwrapped >>= (const result))

returnMT :: (Monad m) => a -> MainControlT m a
returnMT = MainControlT . return . return 

failMT :: (Monad m) => String -> MainControlT m a
failMT = MainControlT . return . fail
 
instance (Monad m) => Monad (MainControlT m) where
  return = returnMT
  (>>=)  = bindMCT
  fail   = failMT

type IOMainControl = MainControlT IO

instance MonadTrans MainControlT where
  lift = MainControlT . liftM return

instance (MonadIO m) => MonadIO (MainControlT m) where
  liftIO  = lift . liftIO 

instance (MonadState s m) => MonadState s (MainControlT m) where
  get = lift get
  put = lift . put

{- I should make this an instance of MonadIO and then do the same thing
   but that would require depending on mtl, not bad but I'll do it when
   I know this will work

liftIO :: IO a -> IOMainControl a
liftIO ioa = MainControlT $ do unwrapped <- ioa
                               return $ return unwrapped
-}

liftMC :: MainControl a -> IOMainControl a
liftMC = MainControlT . return

{- A failed attempt to capture the fact that we often want to
   do IO at the same time as a MainControlling.
   I think instead I need to use monad transformers
newtype IOMainControl a = IOMain (IO (MainControl a))

instance Monad IOMainControl where
  (>>=)  = bindIOControl
  return = return . return
  fail   = return . fail

bindIOControl :: IOMainControl a -> (a -> IOMainControl b) -> IOMainControl b
bindIOControl (IOMain ioma) f =
  IOMain $ do ma <- ioma
              case ma of
                (FatalError err logs')      -> return ma
                (MainResult a warns' logs') -> 
                  case f a of
                    IOMain iomb -> do mb <- iomb
                                      return (ma >>= (const mb))
-}

{-| A function to convert an 'Either' value into a 'MainControl' value.
-}
eitherToMainControl :: Show err => Either err a -> MainControl a
eitherToMainControl (Right a)  = MainResult a [] Map.empty
eitherToMainControl (Left err) = FatalError (show err) Map.empty



{-| Failure, call this function to report an error.
-}
resultError :: IpcError -> MainControl a
resultError err = FatalError err Map.empty

{-| 
  Now we must have a function for placing a value within a "result"
  this is essentially the return method of a monad. However the difference
  is that here we allow the user to add some logging information.
  This puts the given string in at the default logging level.
-}
valueResult :: a
               -- ^ The actual value to store in the result
            -> String
               -- ^ The log key
            -> String
               -- ^ The logging information
            -> MainControl a
               -- ^ The returned result.
valueResult = valueResultLogLevel defaultLogLevel


{-| The same as 'valuseResult' except that you may specify a log level -}
valueResultLogLevel :: Int
                       -- ^ The level at which to log
                    -> a 
                       -- ^ The actual value to store in the result
                    -> String
                       -- ^ The log key
                    -> String
                       -- ^ The logging information
                    -> MainControl a
                       -- ^ The returned result.
valueResultLogLevel level a logName log =
  MainResult a [] $ Map.singleton logName entry
  where
  entry = LogEntry { logEntryData  = log
                   , logEntryLevel = level
                   }

{-| Add log information to a value within a MainControl -}
addLogInformation :: String         -- ^ The log key
                  -> (a -> String)  -- ^ A function to obtain the log info
                  -> MainControl a  -- ^ The result to add log info to
                  -> MainControl a
addLogInformation logKey logFun result =
  do value <- result
     valueResult value logKey (logFun value)


{-| The same as 'valueResult' except the caller provides some warnings
    associated with the value.
-}
resultWarning :: a 
                 -- ^ The actual result
                 -> [ Warning ]
                 -- ^ The warnings
                 -> String
                 -- ^ The log key
                 -> String
                 -- ^ The logging information
                 -> MainControl a
                 -- ^ The returned result
resultWarning = resultWarningLogLevel defaultLogLevel


{-| The same as 'warningResult' except that the user may provide a log level -}
resultWarningLogLevel :: Int 
                         -- ^ The logging level
                      -> a 
                         -- ^ The actual result
                      -> [ Warning ]
                         -- ^ The warnings
                      -> String
                         -- ^ The log key
                      -> String
                         -- ^ The logging information
                      -> MainControl a
                         -- ^ The returned result
resultWarningLogLevel level a warnings logName log = 
  MainResult a warnings $ Map.singleton logName entry
  where
  entry = LogEntry { logEntryData  = log
                   , logEntryLevel = level
                   }

{-| Creates a log entry  from the given log information and log level -}
makeLogEntry :: Int -> String -> LogEntry
makeLogEntry level log = LogEntry { logEntryData  = log
                                  , logEntryLevel = level
                                  }

{-| Creates a 'MainControl' result with the given value, set of warnings,
    and a list mapping log keys to log entries.
-}
logResult :: a -> [ Warning ] -> [ (String, LogEntry) ] -> MainControl a
logResult a warnings = MainResult a warnings . Map.fromList


{-| To exit out of a 'MainControl' we turn it into an 'Either' value where
    we either have some error plus the log or some value plus warnings and logs.
-}
closeOrError :: MainControl a -> Either (IpcError, LogInfo)
                                        (a, [Warning], LogInfo)
closeOrError (FatalError err logS)        = Left  (err, logS)
closeOrError (MainResult a warnings logS) = Right (a, warnings, logS)

{-
  The following are for printing the values held within a 'MainControl'.
  This means that we do not have to come outside of the 'MainControl'
  monad to print, for example the parsed model, to the screen.

  TODO: haddock comment these
-}
showMainControlA :: Show a => MainControl a -> String
showMainControlA (MainResult a _ _)   = show a
showMainControlA (FatalError err _)   = err

displayMainControlA :: Show a => MainControl a -> IO ()
displayMainControlA (MainResult a _ _) = print a
displayMainControlA (FatalError err _) = putStrLn err

showMainControlAWith :: (a -> String) -> MainControl a -> String
showMainControlAWith f (MainResult a _ _)   = f a
showMainControlAWith _ (FatalError err _)   = err

displayMainControlAWith :: (a -> String) -> MainControl a -> IO ()
displayMainControlAWith f (MainResult a _ _) = putStrLn $ f a
displayMainControlAWith _ (FatalError err _) = putStrLn err


-- | Close a main control structure converting the answer with the first
-- given function. The error string is converted to the correct type of
-- answer with the second given function.
closeMainControlWith :: (a -> b) -> (String -> b) -> MainControl a -> b
closeMainControlWith _ f (FatalError err _) = f err
closeMainControlWith f _ (MainResult a _ _) = f a