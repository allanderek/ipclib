module Main
  ( main )
where

{- External Library Module Imports -}
{- Standard Library Module Imports -}
{- Local Module Imports -}
import Ipc.Cli
  (  ipcManual
   , printManual
  )
{- End of Imports -}

{-
  This is the main function which when the file is run as program will generate
  latex source for the manual.
-}
main :: IO ()
main = 
  putStrLn manual
  where
  manual = printManual ipcManual
