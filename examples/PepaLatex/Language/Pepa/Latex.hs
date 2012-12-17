{-|
   This module converts Pepa models into Latex
-}
module Language.Pepa.Latex
    ( modelToLatex )
where


{- Imported Standard Libraries -}
{- Imported External Libraries -}
import Language.Latex.Syntax 
    ( Doc
    , Part                 ( .. )
    , TableRow             ( .. )
    )
{- Imported Local Libraries -}
import Language.Pepa.Syntax
    ( ShowOrig             ( .. )
    , ParsedModel          ( .. )
    , ProcessDef
    , RateSpec
    , RateIdentifier
    , ParsedComponent      ( .. )
    , BeCondition          ( .. )
    , ParsedComponentId
    
    , ParsedAction         ( .. )
    , ParsedRate           ( .. )
    , ParsedRateExp        ( .. )
    , ParsedCond           ( .. )
    )
import Language.Pepa.Print
    ( HumanPrint           ( .. ) )
{- End of Imports -}

{-| Main exported function, takes in a pepa model and returns a
    latex document.
-}
modelToLatex :: ParsedModel -> Doc
modelToLatex (ParsedModel rDefs pDefs mainComp) =
    [ TableEnv "eqnarray*" Nothing allRows ]
    where
    allRows    = rDefRows ++ pDefRows ++ [ systemRow ]
    rDefRows   = map (TableRow . rDefToLatex) rDefs
    pDefRows   = concatMap pDefToLatex pDefs
    systemRow  = TableRow [ PlainText ""
                          , PlainText ""
                          , systemPart 
                          ]
    systemPart = processToLatex mainComp


processNameLatex :: ParsedComponentId -> Part
processNameLatex = PlainText . hprint
   -- LCommand (hprint ident) Nothing []

rateNameLatex :: RateIdentifier -> Part
rateNameLatex = PlainText . showOrig

rDefToLatex :: RateSpec -> [ Part ]
rDefToLatex (rateid, rateExp) =
    [ rateNameLatex rateid
    , LCommand "rmdef" Nothing []
    , MultiPart [ rateExpPart , PlainText ";" ]
    ]
    where
    rateExpPart = rateExpToLatex rateExp

pDefToLatex :: ProcessDef -> [ TableRow ]
pDefToLatex (pid, proc) =
   [ TableRow [ pName
              , rmDef
              , MultiPart[ processPart
                         , PlainText ";"
                         ]
              ]
   ]
   where
   processPart = processToLatex proc
   pName       = processNameLatex pid
   rmDef       = LCommand "rmdef" Nothing []
    


rateToLatex :: ParsedRate -> Part
rateToLatex (RateTimed e)            = rateExpToLatex e
rateToLatex (RateTop)                = LCommand "infty" Nothing []
rateToLatex (RateImmediate Nothing)  = LCommand "immediate" Nothing []
rateToLatex (RateImmediate (Just x)) = 
   LCommand ((show x) ++ ":immediate") Nothing []

rateExpToLatex :: ParsedRateExp -> Part
rateExpToLatex (RateId ident _)         = rateNameLatex ident
rateExpToLatex (RateNum val)            = PlainText $ show val
rateExpToLatex (RateProc ident)         = processNameLatex ident
rateExpToLatex (RateDisabled)           =
   LCommand "0:immediate" Nothing []
rateExpToLatex (RateBexp oper r1 r2) =
    MultiPart [ rateExpToLatex r1
              , PlainText oper
              , rateExpToLatex r2
              ]
rateExpToLatex (RateCond cond r1 r2) =
    MultiPart [ rateExpToLatex r1
              , rateCondToLatex cond
              , rateExpToLatex r2
              ]

rateCondToLatex :: ParsedCond -> Part
rateCondToLatex (PCondId ident)  = processNameLatex ident
rateCondToLatex (PCondSum p1 p2) =
    MultiPart [ rateCondToLatex p1
              , PlainText "+"
              , rateCondToLatex p2
              ]
rateCondToLatex (PCondRate rident operator rExp) =
   MultiPart [ rateNameLatex rident
             , PlainText operator
             , rateExpToLatex rExp
             ]

actionToLatex :: ParsedAction -> Part
actionToLatex = PlainText . hprint

{-
   So currently turning a component into latex produces
   just the one part, but it could in theory return
   a list of parts such that we could later turn them
   into several rows within the latex table.
   I think that is pretty difficult but it may happen
   at somepoint.
-}
processToLatex :: ParsedComponent -> Part
processToLatex ( StopProcess )                        =
   PlainText "Stop"
processToLatex ( IdProcess ident )                    = 
    processNameLatex ident
processToLatex (ComponentSum p1 p2)                   =
    MultiPart [ processToLatex p1
              , PlainText "+"
              , processToLatex p2
              ]
processToLatex (PrefixComponent (act, rate) p)        =
    MultiPart [ PlainText "("
              , actionToLatex act
              , PlainText ", "
              , rateToLatex rate
              , PlainText ")."
              , processToLatex p
              ]
processToLatex (CondBehaviour 
                  (BeCondition rident operator rExp) 
                  p)                                  =
   MultiPart [ PlainText "if"
             , rateNameLatex rident
             , PlainText operator
             , rateExpToLatex rExp
             , PlainText "then"
             , processToLatex p
             ]
processToLatex ( Cooperation p1 actions p2)           =
    MultiPart [ processToLatex p1
              , syncCommand
              , processToLatex p2
              ]
    where
    syncCommand = LCommand "sync" Nothing [ actsPart ]
    actsPart    = MultiSepPart (PlainText ", ") $ map actionToLatex actions
processToLatex (ProcessArray ident i Nothing)         =
    MultiPart [ processNameLatex ident
              , PlainText $ "[" ++ show i ++ "]"
              ]
processToLatex (ProcessArray ident i (Just actions))  =
    MultiPart [ processNameLatex ident
              , PlainText $ "[" ++ show i ++ "]"
              , PlainText "["
              , MultiSepPart (PlainText ", ") $ map actionToLatex actions
              , PlainText "]"
              ]
processToLatex (Hiding p actions)                     =
    MultiPart [ processToLatex p
              , PlainText "/\\{"
              , MultiSepPart (PlainText ", ") $ map actionToLatex actions
              , PlainText "\\}"
              ]
