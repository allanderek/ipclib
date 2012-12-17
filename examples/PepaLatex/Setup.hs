#!/usr/bin/env runhaskell
import Distribution.Simple
import System.Cmd  ( system )
import System.Exit ( exitWith )
import System      ( getArgs )


myHooks :: UserHooks
myHooks = defaultUserHooks


main :: IO ()
main = getArgs >>= processArgs


processArgs :: [ String ] -> IO ()
processArgs _           = defaultMainWithHooks myHooks
