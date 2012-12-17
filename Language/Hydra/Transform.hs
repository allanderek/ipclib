{-# OPTIONS_GHC -XPatternGuards #-}
{-|
   A small module exporting a small number of transformations over hydra
   models.
-}
module Language.Hydra.Transform
   ( translateRates
   , uniquifyTransitions
   , prioritiseTrans
   , transformCExp
   , renameCExp
   , mangleConstants
   , reduceDnamSpeed
   , reduceCExp
   )
where

{- Imported Standard Libraries -}
import Data.List
  ( find )
{- Imported Local Libraries -}
import Language.Pepa.Rates
  ( RateExpr               ( .. )
  , renameRateExpr
  , transformRateExpr
  )
import Language.Hydra.Syntax
   ( DNAMmodel             ( .. )
   , DNAMidentifier
   , DNAMconstantDef       ( .. )
   , DNAMtransition        ( .. )
   , DNAMcondition         ( .. )
   , DNAMspeed             ( .. )
   , DNAMcexp
   , dnamCtrue
   , dnamCfalse
   )
import Language.Pepa.QualifiedName
  ( ShowOrig      ( .. )
  , prefixQName
  )
import Language.Pepa.Syntax
   ( ActionIdentifier
   , ParsedPriority
   , nameOfAction
   , ParsedAction
   )

{- End of Imports -}

{- Translation of a hydra model translating rates -}

{-| Translates the rates in a model according to the given function
    over transitions
-}
translateRates :: (DNAMtransition a b -> DNAMtransition a c)
               -> DNAMmodel a b
               -> DNAMmodel a c
translateRates translate dModel =
   DNAMmodel { modelHeaders     = modelHeaders     dModel 
             , modelStateVector = modelStateVector dModel
             , modelConstants   = modelConstants   dModel
             , modelInitial     = modelInitial     dModel
             , modelTransitions = transitions
             , modelInvariants  = modelInvariants  dModel
             }
   where
   transitions = map translate $ modelTransitions dModel

{-| Makes each transition name unique
   Essentially I believe we need this in order to
   avoid transition name clashes for example in:
   @ P = (a, r).P + (a, r).P @
-}
uniquifyTransitions :: DNAMmodel a b -> DNAMmodel a b
uniquifyTransitions model = 
   model { modelTransitions = newTrans }
   where
   newTrans = map uniqueTrans $ zip [0..] (modelTransitions model)

   uniqueTrans :: (Int, DNAMtransition a b) -> DNAMtransition a b
   uniqueTrans (i, trans) =
      trans { transName = prefixQName (transName trans) $ show i }


{-| Prioritises the given list of action names in the given model -}
prioritiseTrans :: [ (ActionIdentifier, ParsedPriority) ]
                    -- ^ List priorities to apply
                -> DNAMmodel ParsedAction b -- ^ Input model
                -> DNAMmodel ParsedAction b -- ^ The returned model
prioritiseTrans pActions model =
   model { modelTransitions = transitions }
   where
   transitions = map prioritiseTransition $ modelTransitions model

   -- Notice that we check the original names, this means that action
   -- will be prioritised even if it is hidden within the model.
   -- (hmm, actually not entirely sure that's true)
   prioritiseTransition :: DNAMtransition ParsedAction b 
                        -> DNAMtransition ParsedAction b
   prioritiseTransition trans =
     case find isAppropriate pActions of
       Just (_, p)   -> trans { transPriority = p }
       Nothing       -> trans
      where 
      isAppropriate :: (ActionIdentifier, ParsedPriority) -> Bool
      isAppropriate (name, _) = (showOrig name) == actionName
      actionName    = showOrig $ nameOfAction $ transActionKind trans
   


transformCExp :: (DNAMidentifier -> DNAMcexp) -> DNAMcexp -> DNAMcexp
transformCExp = transformRateExpr
   
{-| 
  Rename a C expression, here we take in a dictionary mapping original
  names to Cexpressions and use this to transform a C expression.
  This can be useful when mapping from a an unqualified model to a
  qualified model as we can map the identifier @P@ to @P_1 + P_2 + P_3@
  where those three names are the qualified instances of @P@.
-}
renameCExp :: [ (String, DNAMcexp) ] -> DNAMcexp -> DNAMcexp
renameCExp = renameRateExpr

{-|
  A transformation to mangle the names of constants. This means that they
  will not clash with other names defined in the C++ environment of hydra.

  This function should be generalised to @DNAMmodel a b -> DNAMmodel a b@
  but to do this obviously we require to take in a function to 
  mangle an @a@ and a @b@.
-}
mangleConstants :: DNAMmodel a DNAMspeed -> DNAMmodel a DNAMspeed
mangleConstants dModel =
  DNAMmodel { modelHeaders     = modelHeaders     dModel 
            , modelStateVector = modelStateVector dModel
            , modelConstants   = mangledConstants
            , modelInitial     = modelInitial     dModel
            , modelTransitions = mangledTransitions
            , modelInvariants  = modelInvariants  dModel
            }
  where
  -- build mangler (or scope). This maps names to new names
  mangler :: [ (DNAMidentifier, DNAMidentifier) ]
  mangler = map mkMangleEntry constantDefs
  mkMangleEntry :: DNAMconstantDef -> (DNAMidentifier, DNAMidentifier)
  mkMangleEntry (DNAMconstantDef ident _) = 
    (ident, translateConstantIdentifier ident)

  -- original model constant defs
  constantDefs = modelConstants dModel

  -- mangle all the constants
  mangledConstants = map mangleConstantDef constantDefs

  mangleConstantDef :: DNAMconstantDef -> DNAMconstantDef
  mangleConstantDef (DNAMconstantDef ident cexp) =
    DNAMconstantDef (translateConstantIdentifier ident)
                    (mangleCExp cexp)
  -- mangle all the transitions
  -- Probably should mangle all the model invariants as well.
  mangledTransitions = map mangleTrans $ modelTransitions dModel

  -- mangling a transition involves just mangling the c expressions
  -- of a transition
  mangleTrans :: DNAMtransition a DNAMspeed -> DNAMtransition a DNAMspeed
  mangleTrans trans = 
    trans { transConditions = map mangleCondition $ transConditions trans
          , transSpeed      = mangleSpeed $ transSpeed trans
          }

  mangleCondition :: DNAMcondition -> DNAMcondition
  mangleCondition (DNAMcond cexp)               = DNAMcond $ mangleCExp cexp
  mangleCondition cond@(DNAMprocpresent _ident) = cond

  mangleSpeed :: DNAMspeed -> DNAMspeed
  mangleSpeed (DNAMrate   cexp) = DNAMrate $ mangleCExp cexp
  mangleSpeed (DNAMweight cexp) = DNAMweight $ mangleCExp cexp


  -- mangling a cexpression is just a renamining, we can still use
  -- 'transformCExp' but for that we need to build a function from
  -- names to cexpressions, that's fairly easy using our names to new-names
  -- mapping.
  mangleCExp :: DNAMcexp -> DNAMcexp
  mangleCExp = transformCExp renameIdent
  renameIdent :: DNAMidentifier -> DNAMcexp
  renameIdent ident =
    case lookup ident mangler of
      Nothing       -> Cident ident
      Just newIdent -> Cident newIdent

{-
  Used for mangling rate and other (eg concentrations) constants such
  that they will not clash with c++ constants etc.
-}
translateConstantIdentifier :: DNAMidentifier -> DNAMidentifier
translateConstantIdentifier = (flip prefixQName) "PEPA_CONSTANT__"



{-|
  A wrapper for 'reduceCExp' which operates over the type of 'DNAMspeed'
-}
reduceDnamSpeed :: [ (DNAMidentifier, DNAMcexp) ] -> DNAMspeed -> DNAMspeed
reduceDnamSpeed mapping (DNAMrate rexp) = 
  DNAMrate $ reduceCExp mapping rexp
reduceDnamSpeed mapping (DNAMweight w)  =
  DNAMweight $ reduceCExp mapping w


{-|
  Used for reducing, possibly to a constant, an expression.
  You must provide a mapping between identifiers and c expressions.
-}
reduceCExp :: [ (DNAMidentifier, DNAMcexp) ] -> DNAMcexp -> DNAMcexp
reduceCExp mapping  cexp@(Cident ident)   = 
  -- it's a good question, should we reduce the replaced
  -- expression? it may contain some identifiers in the
  -- mapping, for now I'll say no.
  case lookup ident mapping of
    Nothing -> cexp
    Just e  -> e
reduceCExp _mapping cexp@(Cconstant _)    = cexp
reduceCExp _mapping cexp@(Creal _)        = cexp
reduceCExp mapping  (Cadd e1 e2)          =
  reduceBinaryExp mapping cf df Cadd e1 e2 
  where
  cf :: Int -> Int -> DNAMcexp
  cf x y = Cconstant $ x + y

  df :: Double -> Double -> DNAMcexp
  df x y = Creal $ x + y
reduceCExp mapping  (Csub e1 e2)          =
  reduceBinaryExp mapping cf df Csub e1 e2 
  where
  cf :: Int -> Int -> DNAMcexp
  cf x y = Cconstant $ x - y

  df :: Double -> Double -> DNAMcexp
  df x y = Creal $ x - y 
reduceCExp mapping  (Cmult e1 e2)          =
  reduceBinaryExp mapping cf df Cmult e1 e2 
  where
  cf :: Int -> Int -> DNAMcexp
  cf x y = Cconstant $ x * y

  df :: Double -> Double -> DNAMcexp
  df x y = Creal $ x * y 
reduceCExp mapping  (Cdiv e1 e2)          =
  reduceBinaryExp mapping cf df Cdiv e1 e2 
  where
  -- This is probably a mistake if we decide later that we do want this
  -- then we could maybe have Cdiv (Cconstant x) (Cconstant y)
  -- but I quite like that we're checking for it here.
  cf :: Int -> Int -> DNAMcexp
  cf _x _y = error "cannot divide ints by ints" -- Cconstant $ x / y

  df :: Double -> Double -> DNAMcexp
  df x y = Creal $ x / y
reduceCExp mapping  (Cifte e1 e2 e3)
  | dnamCtrue  == re1 = re2
  | dnamCfalse == re1 = re3
  | otherwise         = Cifte re1 re2 re3
  where
  re1 = reduceCExp mapping e1
  re2 = reduceCExp mapping e2
  re3 = reduceCExp mapping e3

reduceCExp mapping  (Cand e1 e2)
  | dnamCfalse    == re1 = dnamCfalse
  | dnamCfalse    == re2 = dnamCfalse
  | dnamCtrue     == re1 = re2
  | dnamCtrue     == re2 = re1
  | otherwise            = Cand re1 re2
  where 
  re1 = reduceCExp mapping e1
  re2 = reduceCExp mapping e2
reduceCExp mapping  (Cor e1 e2)
  | dnamCtrue  == re1 = dnamCtrue
  | dnamCtrue  == re2 = dnamCtrue
  | dnamCfalse == re1 = re2
  | dnamCfalse == re2 = re1
  | otherwise         = Cor re1 re2
  where 
  re1 = reduceCExp mapping e1
  re2 = reduceCExp mapping e2
reduceCExp mapping  (Cgt e1 e2)            =
  reduceBinaryExp mapping cf df Cgt e1 e2 
  where
  cf :: Int -> Int -> DNAMcexp
  cf x y
    | x > y     = dnamCtrue
    | otherwise = dnamCfalse

  df :: Double -> Double -> DNAMcexp
  df x y
    | x > y     = dnamCtrue
    | otherwise = dnamCfalse
reduceCExp mapping  (Cge e1 e2)            =
  reduceBinaryExp mapping cf df Cge e1 e2 
  where
  cf :: Int -> Int -> DNAMcexp
  cf x y
    | x >= y     = dnamCtrue
    | otherwise  = dnamCfalse

  df :: Double -> Double -> DNAMcexp
  df x y
    | x >= y     = dnamCtrue
    | otherwise  = dnamCfalse
reduceCExp mapping  (Clt e1 e2)            =
  reduceBinaryExp mapping cf df Clt e1 e2 
  where
  cf :: Int -> Int -> DNAMcexp
  cf x y
    | x < y     = dnamCtrue
    | otherwise = dnamCfalse

  df :: Double -> Double -> DNAMcexp
  df x y
    | x < y     = dnamCtrue
    | otherwise = dnamCfalse
reduceCExp mapping  (Cle e1 e2)            =
  reduceBinaryExp mapping cf df Cle e1 e2 
  where
  cf :: Int -> Int -> DNAMcexp
  cf x y
    | x <= y     = dnamCtrue
    | otherwise  = dnamCfalse

  df :: Double -> Double -> DNAMcexp
  df x y
    | x <= y     = dnamCtrue
    | otherwise  = dnamCfalse
reduceCExp mapping  (Ceq e1 e2)            =
  case (reduceCExp mapping e1, reduceCExp mapping e2) of
    (Cconstant i, Cconstant j)
      | i == j     -> dnamCtrue
      | otherwise  -> dnamCfalse
    (Creal i,     Creal j)
      | i == j     -> dnamCtrue
      | otherwise  -> dnamCfalse
    (Creal i,     Cconstant j)
      | i == (fromIntegral j) -> dnamCtrue
      | otherwise             -> dnamCfalse
    (Cconstant i, Creal j)
      | (fromIntegral i) == j -> dnamCtrue
      | otherwise             -> dnamCfalse
    (re1, re2)
      | re1 == re2 -> dnamCtrue
      | otherwise  -> Ceq re1 re2
reduceCExp mapping (Cnot e1)                =
  case reduceCExp mapping e1 of
    Cconstant 0   -> dnamCtrue
    Cconstant _   -> dnamCfalse
    Creal     0.0 -> dnamCtrue
    Creal     _   -> dnamCfalse
    re1           -> Cnot re1
reduceCExp mapping (Cminimum e1 e2)         =
  reduceBinaryExp mapping cf df Cminimum e1 e2 
  where
  cf :: Int -> Int -> DNAMcexp
  cf x y
    | x < y     = Cconstant x
    | otherwise = Cconstant y

  df :: Double -> Double -> DNAMcexp
  df x y
    | x < y     = Creal x
    | otherwise = Creal y
{- LEGACY stuff
reduceCExp mapping (CAppRate e1 e2 e3 e4)
  | Creal l   <- re1
  , Creal m   <- re2
  , Creal raP <- re3
  , Creal raQ <- re4  = Creal $ (l/raP) * (m/raQ) * (min raP raQ)
  | otherwise         = CAppRate re1 re2 re3 re4
  where
  re1 = reduceCExp mapping e1
  re2 = reduceCExp mapping e2
  re3 = reduceCExp mapping e3
  re4 = reduceCExp mapping e4
-- Notice the difference in the reduction expression here for an active
-- passive cooperation, no minimum required.
reduceCExp mapping (CActPass e1 e2 e3 e4)
  | Creal l   <- re1
  , Creal m   <- re2
  , Creal raP <- re3
  , Creal raQ <- re4  = Creal $ (l/raP) * (m/raQ) * raP
  | otherwise         = CActPass re1 re2 re3 re4
  where
  re1 = reduceCExp mapping e1
  re2 = reduceCExp mapping e2
  re3 = reduceCExp mapping e3
  re4 = reduceCExp mapping e4
-}


{-
  hmm an attempt to factor out some of the common code,
  not sure it's particularly prettier.
-}
reduceBinaryExp :: [ (DNAMidentifier, DNAMcexp) ]
                -> (Int -> Int -> DNAMcexp) 
                -> (Double -> Double -> DNAMcexp)
                -> (DNAMcexp -> DNAMcexp -> DNAMcexp)
                -> DNAMcexp 
                -> DNAMcexp 
                -> DNAMcexp
reduceBinaryExp mapping cf df expFn e1 e2 =
  case (reduceCExp mapping e1, reduceCExp mapping e2) of
    (Cconstant cl, Cconstant cr) -> cf cl cr
    (Creal dl, Creal dr)         -> df dl dr
    (Cconstant cl, Creal dr)     -> df (fromIntegral cl) dr
    (Creal dl, Cconstant cr)     -> df dl (fromIntegral cr)
    (re1, re2)                   -> expFn re1 re2
