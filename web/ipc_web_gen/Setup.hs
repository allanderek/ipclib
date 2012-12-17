#!/usr/bin/env runhaskell
import Distribution.Simple
   ( simpleUserHooks
   , UserHooks
   , defaultMainWithHooks   
   )
import System.Cmd  ( system )
import System      ( getArgs )


myHooks :: UserHooks
myHooks = simpleUserHooks


main :: IO ()
main = getArgs >>= processArgs


processArgs :: [ String ] -> IO ()
processArgs ( "happraise" : rest)  =
  do _exitCode    <- system haCommand
     return ()
  where
  haCommand :: String
  haCommand = unwords ("happraise `darcs query manifest`" : rest)
processArgs _           = defaultMainWithHooks myHooks

