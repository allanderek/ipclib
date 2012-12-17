{-|
    This is an example use of the pepa library, it takes in a
    pepa model file and applies some simplifying transformations
    to the model before outputting the same model.
-}

module Main
    ( main )
where

{- Standard library modules imported -}
import Control.Monad
  ( liftM )
import System.Environment
  ( getArgs )
-- import Text.ParserCombinators.Parsec 
--   ( ParseError )
{- External Library modules imported -}
{- Local modules imported -}
import Language.Pepa.Syntax
  ( ParsedModel ) 
import Language.Pepa.FileParser
  ( parsePepaFile )
import Language.Pepa.Print
  ( hprintPepaModel )
import Language.Pepa.Transform.Simplify
  ( simplify
  , SimplifyResult ( .. )
  )
import Language.Pepa.MainControl
  ( runMainControlT
  , closeMainControlWith
  )
{- End of module imports -}


main :: IO ()
main = getArgs >>= processArgs
   
-- Very simplestic options parsing there is no need to go to the
-- length of using 'System.GetOpt' since so far we only have one option.
-- In the future probably the whole library will have a common set of
-- command-line options and we can use that function to parse them.       
processArgs :: [ String ] -> IO () 
processArgs []                          = 
    putStrLn "No input files to analyse"
processArgs ("--print-pepa" : files)    = 
    mapM_ (convertFile [] True)  files
processArgs files                       = 
    mapM_ (convertFile [] False) files

convertFile :: [ String ] -> Bool -> FilePath -> IO ()
convertFile _args printPepa file = 
    do parseResult <- runMainControlT $ parsePepaFile file
       let sResult = do pModel <- parseResult
                        sModel <- liftM simplifiedModel $ simplify [] pModel
                        return (pModel, sModel)
           output  = closeMainControlWith displaySimplified id sResult
       putStrLn output
    where
    displaySimplified :: (ParsedModel, ParsedModel) -> String
    displaySimplified (pModel, sModel)
      | not printPepa = hprintPepaModel sModel
      | otherwise     = 
        unlines [ "---- The Original Model ----"
                , hprintPepaModel pModel
                , "---- The Simplified Model ----"
                , hprintPepaModel sModel
                ]


