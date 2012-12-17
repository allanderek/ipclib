module Language.Pepa.Ode.OdeGeneration
    ( toOdeSystem 
    , formatOdeSystem
    )
where
{- Standard Library Modules Imported -}
{- Non-Standard Library Modules Imported -}
import Data.List                     ( find
                                      , nub
                                      )
import Data.Map                      ( toList )
import Data.Maybe                    ( mapMaybe )
{- Local Library Modules Imported -}
import Language.Pepa.Utils                    ( mkSepList )
import Language.Pepa.Syntax                   ( QualifiedName      ( .. )
                                              , qualifyQName
                                              , getOrigName
                                              , ShowOrig            ( .. )
                                              , ParsedModel
                                              , ParsedComponentId
                                              , ParsedTrans
                                              , ParsedAction
                                              , RateSpec
                                              , ConcentrationSpec
                                              )
import Language.Pepa.ApparentRates           ( summedPepaRate )
import Language.Pepa.PepaUtils               ( Transition
                                              , possibleTransSuccessors
                                              , possibleTransitions
                                              , isPepaActionEnabled
                                              )
import Language.Pepa.Print                   ( HumanPrint ( .. ) )
import Language.Pepa.Ode.TimedSystemEquation ( TimedSystem
                                              , TimedSystemEqn     ( .. )
                                              , NVFcomp            ( .. )
                                              , getNvfIdentifier
                                              , formatNumericalForm
                                              , TSEcptExpression   ( .. )
                                              , formatTseCompExpr
                                              , apparentTSErate
                                              )
{- End of Module Imports -}

----------------
-- Data Types --
----------------

{-
The use of the term Ode before the data structures is just symbolic and to maintain 
coherence in this file. Hence do not be put off by the fact that we have an
equation equation type in OdeEquation!
-}

type OdeCptType = QualifiedName
type OdeVariable = NVFcomp
type OdeCRresult = TSEcptExpression

type OdeInitRate = RateSpec --TimedSystemInitRate
type OdeInitCptConfig = ConcentrationSpec --TimedSystemInitCptConf
data OdeInitConstDefns = ODEinitConsts [OdeInitRate] [OdeInitCptConfig]


data OdeNuResult = NROne | NRvar OdeVariable

data OdeEquationPartElem 
    = OPtEFull OdeCRresult OdeNuResult
    | OPtECR OdeCRresult
    | OPtZero
data OdeEquationPart = OdeIn   [ OdeEquationPartElem ]
                      | OdeOut [ OdeEquationPartElem ]
data OdeEquation = OSum OdeCptType OdeEquationPart OdeEquationPart

data OdeSystem = ODEsys OdeInitConstDefns [OdeEquation]


---------------
-- Instances --
---------------
{-
instance Eq OdeNuResult where
    (==) NROne NROne = True
    (==) (NRvar nvf1) (NRvar nvf2) = nvf1==nvf2

instance Simplify OdeEquation where
    simplify (OSum cq p1 p2) = OSum cq (simplify p1) (simplify p2)

instance Simplify OdeEquationPart where
    simplify (OPt sign l) = OPt sign (map simplify l) 

instance Simplify OdeEquationPartElem where
    simplify (OPtEFull cr nu) 
        | (nu == NROne) = OPtECR (simplify cr) -- x * 1 = x
        | ((simplify cr) == TSEzero) = OPtZero -- 0 * x = 0
        | otherwise = OPtEFull (simplify cr) nu 
    simplify (OPtECR cr) = OPtECR (simplify cr)
    simplify OPtZero = OPtZero

instance Show OdeSystem where
    show (ODEsys consts eqns) = (show consts)++"\n"++(show eqns)

instance Show OdeInitConstDefns where
    show (ODEinitConsts rates cpts) = (listToStringListWithNewline rates)
        ++ "\n"++(listToStringListWithNewline cpts)

instance Show OdeEquation where
    show (OSum c incom outgo) 
        = "\nd/dt( "++(show c)++" ) = "
          ++ (show outgo) ++ (show incom)
          ++ "\n"

instance Show OdeEquationPart where
    show (OPt OdeIn l) = showOEPElist l "+"
    show (OPt OdeOut l) = showOEPElist l "-"

instance Show OdeEquationPartElem where
    show (OPtEFull cr cc) = (show cr)++" * "++(show cc)
    show (OPtECR cr) = show cr
    show OPtZero = "0"

instance Show OdeNuResult where
    show NROne = "1" 
    show (NRvar n) = show n
-}  

formatOdeSystem :: OdeSystem -> String
formatOdeSystem (ODEsys consts equations) =
    unlines $ (formatOdeConstants consts) :
              (map formatOdeEquation equations)

formatOdeConstants :: OdeInitConstDefns -> String
formatOdeConstants (ODEinitConsts rates concentrations) =
    ""

formatOdeEquation :: OdeEquation -> String
formatOdeEquation (OSum ident incom outgo) =
    unwords [ "d/dt("
            , hprint ident
            , ") ="
            , formatOdeEquationPart outgo
            , formatOdeEquationPart incom
            ]

formatOdeEquationPart :: OdeEquationPart -> String
formatOdeEquationPart (OdeIn  l) = formatElements "+" l
formatOdeEquationPart (OdeOut l) = formatElements "-" l

formatElements :: String -> [ OdeEquationPartElem ] -> String
formatElements sign = mkSepList sign . map formatOdeElement

formatOdeElement :: OdeEquationPartElem -> String
formatOdeElement (OPtECR cr)      = formatTseCompExpr cr
formatOdeElement (OPtZero)        = "0"
formatOdeElement (OPtEFull cr cc) =
    unwords [ formatTseCompExpr cr
            , "*"
            , formatOdeNuResult cc
            ]

formatOdeNuResult :: OdeNuResult -> String
formatOdeNuResult (NROne)   = "1"
formatOdeNuResult (NRvar n) = formatNumericalForm n

--------------------
-- Main Functions --
--------------------


toOdeSystem :: TimedSystem -> ParsedModel -> OdeSystem
toOdeSystem (rateSpecs, concentrations, tseqn) model =
    ODEsys consts equations
    where
    consts    = ODEinitConsts rateSpecs $ toList concentrations
    equations = toOdeEquations tseqn model

-- The use of 'toOdeEquationsSub' auxiliary function is so that the
-- entire initial 'TimedSystemEqn' can be maintained (and hence
-- referenced) during the translation of a Numerical Vector Form
toOdeEquations :: TimedSystemEqn -> ParsedModel -> [OdeEquation]
toOdeEquations sys model = 
    toOdeEquationsSub sys
    where
    toOdeEquationsSub :: TimedSystemEqn -> [OdeEquation]
    toOdeEquationsSub (TSECooperation ts1 l ts2) = 
        (toOdeEquationsSub ts1) ++ (toOdeEquationsSub ts2) 
    toOdeEquationsSub (TSEHide ts l)             = 
        toOdeEquationsSub ts 
    toOdeEquationsSub (TSENumVerForm nvfs l)      = 
        map (getOdeFromNvfCpt nvfs) nvfs

    getOdeFromNvfCpt :: [ NVFcomp ] -> NVFcomp -> OdeEquation
    getOdeFromNvfCpt nvfs n = 
        OSum qc incomingPt outgoingPt 
        where
        -- G'ah not happy about this pattern here.(took it out, but..)
        qc        = getNvfIdentifier n
        -- Okay so now I have taken out the irrefutable pattern
        -- but I don't really like this much better
        (c, i)    = case qc of
                        Qualified c' [] -> (c', 0)
                        Qualified c' is -> (c', head is)
                        _               -> (getOrigName qc, 0)

        incomingPt = OdeIn  $ getIncoming nvfs qc i
        outgoingPt = OdeOut $ getOutgoing qc
        
    getIncoming :: [ NVFcomp ] -> ParsedComponentId -> Int
                -> [ OdeEquationPartElem ]
    getIncoming nvfs c i = 
        map getOdePtElem incomingList
        where
        -- Build a list of all transitions of cpts in the nvf
        incomingList = nub (foldr1 (++) (map getLeadsHere nvfs))
        
        getLeadsHere :: NVFcomp -> [ (ParsedAction, ParsedComponentId) ]
        getLeadsHere comp =
            map getincptlist $ filter leadsHere trans
            where
            ident = getNvfIdentifier comp
            trans = possibleTransSuccessors model ident
            
            leadsHere :: Transition -> Bool
            leadsHere (_, Just c2) = c == c2
            leadsHere (_, Nothing) = False             

            getincptlist :: Transition -> (ParsedAction, ParsedComponentId)
            getincptlist (t, _) = (extractAction t, qualifyQName ident i) 

    getOutgoing :: ParsedComponentId -> [OdeEquationPartElem] 
    getOutgoing c = 
        map getOdePtElem outcptlist
        where
        outcptlist = map getoutcpt translist
        translist = possibleTransitions model c

        getoutcpt :: ParsedTrans -> (ParsedAction, ParsedComponentId)
        getoutcpt t = (extractAction t, c)

    getOdePtElem :: (ParsedAction, ParsedComponentId) -> OdeEquationPartElem
    getOdePtElem (a, c') = 
        OPtEFull odResult nuResult
        where
        odResult = cptRateFunc a c' model sys
        nuResult = cptCountingFunc a c' sys

    extractAction :: ParsedTrans -> ParsedAction
    extractAction = fst


-- \rho_a(P) function from eqn (6) of Bradley/Hillston 2006
{- Again this uses 'Language.Pepa.ApparentRates.summedPepaRate'
   but it should probably use
   'Language.Pepa.ApparentRates.apparentPepaRate'
-}
cptRateFunc :: ParsedAction -> OdeCptType -> ParsedModel 
            -> TimedSystemEqn -> OdeCRresult
cptRateFunc a cq model (TSECooperation ts1 l ts2)
    | (elem a l) && (elem cq va1)      = 
        TSEmin (cptRateFunc a cq model ts1) (apparentTSErate a ts2 model)
    | (elem a l) && (elem cq va2)      = 
        TSEmin (cptRateFunc a cq model ts2) (apparentTSErate a ts1 model)
    | (elem a l) && (elem cq i1) 
      && (not $ elem cq va1)           = cptRateFunc a cq model ts1
    | (elem a l) && (elem cq i2) 
      && (not $ elem cq va2)           = cptRateFunc a cq model ts2
    | (not $ elem a l) && (elem cq i1) = cptRateFunc a cq model ts1 
    | (not $ elem a l) && (elem cq i2) = cptRateFunc a cq model ts2 
    | otherwise = TSEzero
    where
        cunq = showOrig cq
        va1 = tseV a ts1 model
        va2 = tseV a ts2 model
        i1 = tseI ts1
        i2 = tseI ts2       
cptRateFunc a cq model (TSEHide ts l)
    | elem cq i = cptRateFunc a cq model ts
    | otherwise = TSEzero
    where
        cunq = showOrig cq
        i = tseI ts
cptRateFunc a cq model ts@(TSENumVerForm _nvf l)
    | elem a l = apparentTSErate a ts model
    | otherwise = TSEatom cq $ summedPepaRate a cq model


-- \nu_a function from eqn (9) of Bradley/Hillston 2006
cptCountingFunc :: ParsedAction -> OdeCptType -> TimedSystemEqn -> OdeNuResult
cptCountingFunc a cq (TSECooperation ts1 l ts2)
    | elem ({-showOrig-} cq) (tseI ts1) = cptCountingFunc a cq ts1
    | elem ({-showOrig-} cq) (tseI ts2) = cptCountingFunc a cq ts2
    | otherwise                         = NROne
cptCountingFunc a cq (TSEHide ts acts)  = cptCountingFunc a cq ts
cptCountingFunc a cq (TSENumVerForm nvf acts)
    | elem a acts = case findNVFcptFromQualCpt cq nvf of
                        Just c  -> NRvar c
                        Nothing -> error errorMessage
    | otherwise   = NROne
    where
    -- Okay this is not a good error message, fix it.
    errorMessage = "component not found despite the action being found"


-- \cI function from eqn (7) of Bradley/Hillston 2006
tseI :: TimedSystemEqn -> [ ParsedComponentId ]
tseI (TSECooperation t1 _ t2) = (tseI t1) ++ (tseI t2)
tseI (TSEHide t _)            = tseI t
tseI (TSENumVerForm cs _)     = map getNvfIdentifier cs --map showOrig cs


-- \cV_a function from eqn (8) of Bradley/Hillston 2006
-- The final case here must return all the components in the numerical
-- vector form, for which the given action is enabled.
tseV :: ParsedAction -> TimedSystemEqn -> ParsedModel -> [ ParsedComponentId ]
tseV a (TSECooperation t1 _ t2) model = 
    (tseV a t1 model) ++ (tseV a t2 model)
tseV a (TSEHide t actions) model
    | elem a actions = []
    | otherwise      = tseV a t model
tseV a (TSENumVerForm cs _) model     =
    mapMaybe getEnabledComponent cs
    where
    getEnabledComponent :: NVFcomp -> Maybe ParsedComponentId
    getEnabledComponent (NVFcomp _count ident)
        | isPepaActionEnabled a ident model = Just ident
        | otherwise                         = Nothing

-------------------------
--- Utility Functions ---
-------------------------


-- Returns the NVFcpt out of a list, who's qualified comopnent is a given one
findNVFcptFromQualCpt :: OdeCptType -> [ NVFcomp ] -> Maybe NVFcomp
findNVFcptFromQualCpt cq = find ((cq ==) . getNvfIdentifier)
{-
findNVFcptFromQualCpt cq [] = error ("PepaOdeGeneration.cptCountingFunc.findNVFcptFromQualCpt: "
                            ++ "Component "++(show cq)++" not found in list of NVF components")
findNVFcptFromQualCpt cq (n:nvfs) = (cq == (getNVFqualifiedCpt n)) ? (n, findNVFcptFromQualCpt cq nvfs)
-}

{-
showOEPElist :: [OdeEquationPartElem] -> String -> String
showOEPElist [] _ = "" 
showOEPElist (x:xs) sign = "\n\t"++sign++"("++(show x)++")" ++ showOEPElist xs sign


simplifyODEsys :: OdeSystem -> OdeSystem
simplifyODEsys (ODEsys consts eqns) = ODEsys consts (map simplify eqns)


-- This could be more elaborate if one would want to output in some specific format
forFile :: OdeSystem -> String
forFile sys = show sys


analyseODEsys :: OdeSystem -> String
analyseODEsys (ODEsys consts eqns) = "System contains "++(show $ length eqns)++" ODEs"
        ++ "\n\nUnsimplified version of the equations:\n\n"++(show eqns)

-}