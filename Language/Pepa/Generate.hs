{-|
    Utilities to assist in the generation of pepa models.
    This means large pepa models can be generated from a smaller
    description. For example a counter with 100 states which are
    all more or less the same can perhaps be more usefully
    generated.
-}
module Language.Pepa.Generate
    ( PepaCounter ( .. ) 
    , counterToPepa

    , testCounter
    , testOne
    ) 
where

{- Imported Standard Libraries -}
import qualified Data.Maybe as Maybe
import Text.ParserCombinators.Parsec
    ( parse
    , ParseError
    )
{- Imported Local Libraries -}
import Language.Pepa.Syntax
    ( ParsedComponent     ( .. )
    , ParsedComponentId
    , ProcessDef
    , QualifiedName       ( .. )
    , ActionIdentifier
    , ParsedAction        ( .. )
    , ParsedRateExp       ( .. )
    , RateWeight
    , ParsedValue
    )
import Language.Pepa.Print
    ( hprintProcessDef )
import Language.Pepa.Parser
    ( pepaComponent )
import Language.Pepa.PepaUtils
    ( makePepaChoice )
import Language.Pepa.Utils
    ( mapFst )
import Language.Pepa.Transform.Replace
    ( ComponentMap
    , replaceComponentNames
    )
{- End of Imports -}

{-| 
   A 'PepaCounter' is a set of definitions which define a counter process.
   A counter process has a high state and a low state and many states 
   in between. The process does the same thing in all of the states in
   between modified only slightly by the transitions up and down the
   counter.
-}
data PepaCounter = 
    PepaCounter { -- | The name prefixed to all definitions
                  pcName   :: String
                  -- | The starting component
                , pcStart  :: ParsedComponent
                  -- | The ending component
                , pcEnd    :: ParsedComponent
                  -- | see 'ProcessMapGenerator'
                , pcNs     :: ProcessMapGenerator
                  -- | The general form of component within the counter
                , pcMiddle :: ParsedComponent
                  -- | The number of definitions in the counter.
                , pcNumber :: Int
                }

{-|
  The type of a generator for a process name mapping. 
  In a counter definition one may use a name such as "N"
  This name is then mapped to a function from ints to ints.
  Where it is used in the counter definition it then means
  "take the current value of the counter, apply the function to it
   and then make the appropriate component name from that number".

  So for example we may define "N" as @(1+)@
  In our counter definition we will have something like:
  @(increment, r).N@
  Suppose the counter is named "Alice" then this will be expanded
  in "Alice10" as:
  @Alice10 = (increment, r).Alice11@
-}
type ProcessMapGenerator = [ (ParsedComponentId,  Int -> Int) ]


{-| 
    'counterToPepa' takes a 'PepaCounter' and produces a normal
    pepa component by expanding the counter component.
  
    [@todo@] we should check if the number given is actually less than zero.
-}
counterToPepa :: PepaCounter -> [ ProcessDef ]
counterToPepa pepaCounter =
    firstDef : (makeDefinitions (totalDefs - 1))
    where
    firstDef  = (makeDefName (show totalDefs)
                , replaceComponentNames (makeMapping totalDefs)
                  $ pcStart pepaCounter
                )
    totalDefs = pcNumber pepaCounter 
    prefix    = pcName pepaCounter    

    makeDefName :: String -> QualifiedName
    makeDefName = Unqualified . (prefix ++)

    makeDefinitions :: Int -> [ ProcessDef ]
    makeDefinitions 0 = [ ( makeDefName "0"
                          , replaceComponentNames (makeMapping 0)
                            $ pcEnd pepaCounter
                          )
                        ]
    makeDefinitions x = 
        (makeDefName (show x), component) :
        (makeDefinitions $ x - 1)
        where
        component = replaceComponentNames (makeMapping x) $
                    pcMiddle pepaCounter
 
    -- Takes a map generator and makes a mapping
    -- based on a given number in the counter
    makeMapping :: Int -> ComponentMap
    makeMapping i = map (\(p, f) -> (p, IdProcess $ makeDefName $ show (f i)))
                    $ pcNs pepaCounter


{-| 
    Simply a function for testing counters. It converts the counter into
    a set of definitions and then prints that model.
-}
testCounter :: PepaCounter -> String
testCounter =
    unlines . map hprintProcessDef . counterToPepa


testCocome :: String
testCocome =
    unlines [ testCounter testCocomeCounter
            , makeCustomerChoice 1 8
            , makeCustomerChoice 9 15
            , makeCustomerChoice 16 25
            , makeCustomerChoice 26 50
            , makeCustomerChoice 51 75
            , makeCustomerChoice 76 100
            , productsChoice -- should go higher.
            , cashierScan
            , custPayCash
            , cashierGiveChange
            ]
    where
    -- Actions 
    tauAct     = Unqualified "tau"
    payCashAct = Unqualified "payCash"
    giveChange = Unqualified "giveChange"
 
    -- Processes
    cashierTwo     = IdProcess $ Unqualified "Cashier2"
    cashierReceipt = IdProcess $ Unqualified "CashierReceipt"
    customerWait   = IdProcess $ Unqualified "CustomerWait"

    -- Definition strings
    cashierScan :: String
    cashierScan = 
        hprintProcessDef ( Unqualified "CashierScan"
                         , makeHistogramChoice tauAct tauAct 
                              choices cashierTwo
                         )
        where
        choices = [ ( 0.9 , 3.0 )
                  , ( 0.05, 1.0 )
                  , ( 0.04, 0.5 )
                  , ( 0.01, 0.2 )
                  ]

    custPayCash :: String
    custPayCash = 
        hprintProcessDef ( Unqualified "CustomerPayCash"
                         , makeHistogramChoice tauAct payCashAct
                              choices customerWait
                         )
        where
        choices = [ ( 0.3 , 5.0  )
                  , ( 0.5 , 8.0  )
                  , ( 0.2 , 10.0 )
                  ]

    cashierGiveChange :: String
    cashierGiveChange = 
        hprintProcessDef ( Unqualified "CashierGiveChange"
                         , makeHistogramChoice tauAct giveChange
                              choices cashierReceipt
                         )
        where
        choices = [ ( 0.2 , 3.0 )
                  , ( 0.6 , 4.0 )
                  , ( 0.2 , 5.0 )
                  ]


    productsChoice :: String
    productsChoice =
        hprintProcessDef ( Unqualified "CustomerItems"
                         , makeUnevenChoice tauAct pNames
                         )
        where 
        pNames = map (mapFst Unqualified) 
                    [ ( "Customer1to8",    0.3  )
                    , ( "Customer9to15",   0.1  )
                    , ( "Customer16to25",  0.15 )
                    , ( "Customer26to50",  0.15 )
                    , ( "Customer51to75",  0.2  )
                    , ( "Customer76to100", 0.1  )
                    ]

    makeCustomerChoice :: Int -> Int -> String
    makeCustomerChoice x y = 
        hprintProcessDef (Unqualified defName, process)
        where
        defName     = concat [ "Customer"
                             , show x
                             , "to"
                             , show y
                             ]
        process     = mkEqualChoice tauAct choiceNames
        choiceNames = map (("Customer" ++) . show) $ enumFromTo x y

    -- Just an alias to 'equalChoice' that also applies 'Unqualfied'
    -- to the names gives.
    mkEqualChoice :: ActionIdentifier -> [ String ]-> ParsedComponent
    mkEqualChoice act = (equalChoice act) . (map Unqualified)

type UnevenDist = [ (Int, Int, Int) ]

testCocomeUnpleasant :: String
testCocomeUnpleasant =
    unlines [ unlines $ map hprintProcessDef custItemsDefs ]
    where
    -- Definitions
    custItemsDefs = makeCustomerDefs distribution
    distribution  = [ (1, 2, 1)
                    , (3, 4, 9)
                    ]
--     distribution  = [ (1,  8,   6)
--                     , (9,  15,  2)
--                     , (16, 25,  3)
--                     , (26, 50,  3)
--                     , (51, 75,  4)
--                     , (76, 100, 2)
--                     ]

    -- Actions 
    tauAct     = Unqualified "tau"
    scanItem   = Unqualified "scanItem"
   
    -- transitions
    scanTrans  = ( Action scanItem, RateTop )
 
    -- Processes
    customerZero = IdProcess $ Unqualified "Customer0"

    -- A new make customer def which works for the uneven distribution
    -- as in the proper cocomeExample
    makeCustomerDefs :: UnevenDist -> [ ProcessDef ]
    makeCustomerDefs = Maybe.concatMap makeRange

    makeRange :: (Int, Int, Int) -> [ ProcessDef ]
    makeRange (start, stop, weight) =
        makeRangeCustomerDefs start (stop - start, stop, weight)

    -- Make the main customer definitions
    makeRangeCustomerDefs :: Int -> (Int, Int, Int) -> [ ProcessDef ]
    makeRangeCustomerDefs current (len, stop, weight)
        | current <= stop   = scanDef : nextDef : restDefs
        | otherwise       = []
        where
        restDefs = makeRangeCustomerDefs (current + 1) (len, stop, weight)
        scanDef  = ( Unqualified $ "Customer" ++ (show current)
                   , PrefixComponent scanTrans $ IdProcess nextName
                   )
        nextName     = Unqualified $ "Customer" ++ (show current) ++ "next"
        nextDef      = ( nextName
                       , ComponentSum stopProcess keepGoing
                       )
        stopProcess  = PrefixComponent (Action tauAct, RateImmediate $ Just 1.0)
                                       customerZero
        keepGoing    = PrefixComponent (Action tauAct, RateImmediate probability)
                                       nPlusOneProc
        nPlusOneProc = IdProcess $ Unqualified $ "Customer" ++ (show $ current + 1 )
        probability  = Just $ ( (fromIntegral len) * 
                                (fromIntegral weight)
                              ) - 1 - ( fromIntegral $ current - (stop - len) )
                       


    -- Just an alias to 'equalChoice' that also applies 'Unqualfied'
    -- to the names gives.
    mkEqualChoice :: ActionIdentifier -> [ String ]-> ParsedComponent
    mkEqualChoice act = (equalChoice act) . (map Unqualified)

testCocomeChoice :: String
testCocomeChoice =
    hprintProcessDef ( Unqualified "Customer1to8", choiceComp )
    where
    choiceComp = equalChoice (Unqualified "tau") idents
    idents     = map Unqualified
                    [ "Customer8"
                    , "Customer7"
                    , "Customer6"
                    , "Customer5"
                    , "Customer4"
                    , "Customer3"
                    , "Customer2"
                    , "Customer1"
                    ]

testOne :: PepaCounter
testOne =
    PepaCounter { pcName   = "FirstCounter"
                , pcStart  = startComponent
                , pcEnd    = endComponent
                , pcNs     = mapGenerator
                , pcMiddle = middleComponent
                , pcNumber = 10
                }
    where
    startComponent  = getComponent "(decrement, r).M"
    middleComponent = getComponent "(increment, r).N + (decrement, r).M"
    endComponent    = getComponent "(increment, r).N"

    mapGenerator    = [ ( Unqualified "N", (1+) )
                      , ( Unqualified "M", (\x -> x - 1) )
                      ]

    getComponent :: String -> ParsedComponent
    getComponent = takeFromParseResult . (parse pepaComponent "no file")

    takeFromParseResult :: Either ParseError a -> a
    takeFromParseResult (Left err) = error $ show err
    takeFromParseResult (Right a ) = a

testCocomeCounter :: PepaCounter
testCocomeCounter =     
    PepaCounter { pcName   = "Customer"
                , pcStart  = startComponent
                , pcEnd    = endComponent
                , pcNs     = mapGenerator
                , pcMiddle = middleComponent
                , pcNumber = 100
                }
    where
    startComponent  = getComponent "(scanItem, infty).M"
    middleComponent = getComponent "(scanItem, infty).M"
    endComponent    = getComponent "(pressEnd, infty).CustomerPay "

    mapGenerator    = [ ( Unqualified "M", (\x -> x - 1) ) ]

    getComponent :: String -> ParsedComponent
    getComponent = takeFromParseResult . (parse pepaComponent "no file")

    takeFromParseResult :: Either ParseError a -> a
    takeFromParseResult (Left err) = error $ show err
    takeFromParseResult (Right a ) = a

testCocomeUnpleasantCounter :: PepaCounter
testCocomeUnpleasantCounter = 
    PepaCounter { pcName   = "Customer"
                , pcStart  = startComponent
                , pcEnd    = endComponent
                , pcNs     = mapGenerator
                , pcMiddle = middleComponent
                , pcNumber = 100
                }
    where
    startComponent  = getComponent 
        "(scanItem, infty).( (tau, 1:immediate).Customer0 + (tau, 1:immediate).M )"
    middleComponent = getComponent 
        "(scanItem, infty).( (tau, 1:immediate).Customer0 + (tau, 1:immediate).M )"
    endComponent    = getComponent "(pressEnd, infty).CustomerPay "

    mapGenerator    = [ ( Unqualified "M", (\x -> x - 1) ) ]


{- A simple utility to turn a string into a component via the parser.
-}
getComponent :: String -> ParsedComponent
getComponent =
    takeFromParseResult . (parse pepaComponent "no file")
    where
    takeFromParseResult :: Either ParseError a -> a
    takeFromParseResult (Left err) = error $ show err
    takeFromParseResult (Right a ) = a

    
    
{-| A convenient way to create counters 
    Actually though, I'm not sure about this, I'm not sure it doesn't
    just take away a bunch of the typing.
-}
createCounter :: String -> -- | The name of the counter 
                 String -> -- | The n + 1 component name
                 String -> -- | The n - 1 component name
                 String -> -- | The start component
                 String -> -- | The end component
                 String -> -- | The middle component
                 Int    -> -- | The number of components
                 PepaCounter
createCounter counterName n m start end middle number =
    PepaCounter { pcName   = counterName
                , pcStart  = getComponent start
                , pcEnd    = getComponent end
                , pcNs     = mapGenerator
                , pcMiddle = getComponent middle
                , pcNumber = number
                }
    where
    mapGenerator    = [ ( Unqualified n, (1+) )
                      , ( Unqualified m, (\x -> x - 1) )
                      ]

    getComponent :: String -> ParsedComponent
    getComponent = takeFromParseResult . (parse pepaComponent "no file")

    takeFromParseResult :: Either ParseError a -> a
    takeFromParseResult (Left err) = error $ show err
    takeFromParseResult (Right a ) = a


{-| Creats a component which has equal choice of immediately becoming one
    of a number of other components. The components are listed and the
    name of the immediate action to take is given.
-}
equalChoice :: ActionIdentifier -> [ ParsedComponentId ] -> ParsedComponent
equalChoice action idents =
    makePepaChoice $ map makePrefix idents
    where
    makePrefix :: ParsedComponentId -> ParsedComponent
    makePrefix = 
        PrefixComponent (Action action, RateImmediate Nothing) . IdProcess


{-| Creates a component choice out of a a list of probabilities. -}
makeUnevenChoice :: ActionIdentifier -> [ (ParsedComponentId, RateWeight) ]
                 -> ParsedComponent
makeUnevenChoice action idWeights =
    makePepaChoice $ map makePrefix idWeights
    where
    makePrefix :: ( ParsedComponentId, RateWeight)  -> ParsedComponent
    makePrefix (p,w) = 
        PrefixComponent  (Action action, RateImmediate (Just w)) $ IdProcess p

{-| Histogram duration, this is for a distribution which is not exponential.
    We generate an immediate choice representing the possible rates
    of the activity.
-}
makeHistogramChoice :: ActionIdentifier -> ActionIdentifier
                    -> [ ( RateWeight, ParsedValue ) ]
                    -> ParsedComponent -> ParsedComponent
makeHistogramChoice choiceAct action choices nextP =
    makePepaChoice $ map makePrefix choices
    where
    makePrefix :: ( RateWeight, ParsedValue ) -> ParsedComponent
    makePrefix (w, r) = 
        PrefixComponent (Action choiceAct, RateImmediate $ Just w) $
        PrefixComponent (Action action, RateNum r) nextP