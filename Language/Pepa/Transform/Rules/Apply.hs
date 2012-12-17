module Language.Pepa.Transform.Rules.Apply
  ( applyRulesToModel )
where

{- Standard Library Modules Imported -}
import Control.Monad
  ( foldM )
import qualified Data.Map as Map
import Data.Map
  ( Map )
{- External Library Modules Imported -}
{- Local Modules Imported -}
import Language.Pepa.Transform.Rules.Syntax
  ( Rules
  , Rule           ( .. )
  , ComponentName
  , Pattern        ( .. )
  , ActionsPattern ( .. )
  , PatternVariable
  , ExprPattern    ( .. )
  , Replacement
  , patternVariables
  , replacementVariables
  )
import qualified Language.Pepa.QualifiedName as QualifiedName
import Language.Pepa.QualifiedName
  ( QualifiedName  ( .. ) )
import Language.Pepa.Rates
  ( RateExpr       ( .. ) )
import qualified Language.Pepa.Syntax as Pepa
import Language.Pepa.Syntax
  ( ParsedModel     ( .. )
  , ParsedComponent ( .. )
  , CooperationSet  ( .. )
  , ParsedAction    ( .. )
  , nameOfAction
  )
import qualified Language.Pepa.Print as PepaPrint
import qualified Language.Pepa.MainControl as MainControl
import Language.Pepa.MainControl
  ( MainControl )
import qualified Language.Pepa.Utils as Utils
import qualified Language.Pepa.Compile.PartEval as PartEval
{- End of Module Imports -}



{-|
   Apply a set of transformation rules to a model
-}
applyRulesToModel :: ParsedModel -> Rules -> MainControl ParsedModel
applyRulesToModel pModel rules = 
  do tModel <- foldM applyRuleToModel pModel rules
     let logKey  = "transformed-model"
         logInfo = PepaPrint.hprintPepaModel tModel
     MainControl.valueResult tModel logKey logInfo 

{-| Apply a single transformation rule to a model -}
applyRuleToModel :: ParsedModel -> Rule -> MainControl ParsedModel
applyRuleToModel pModel rule
  | not $ null undefinedVars = 
    MainControl.resultError undefinedErr
  | otherwise                =
    do newModel <- mTransformed
       -- So if we managed to apply the rule to the model
       -- (if we didn't the mTransformed will just be a 'failed'
       -- main control and this bit doesn't get evaluated)
       -- then we must return a model which contains all the
       -- original definitions plus the new ones. In addition
       -- system equation should be the new one, so we use 'const'
       -- here and give the newModel first.
       return $ Pepa.combineModels const newModel pModel
  where
  mTransformed  = applyRule $ modelSystemEqn pModel
  pattern       = rulePattern rule
  replacement   = ruleReplace rule
  patternVars   = patternVariables pattern
  replaceVars   = replacementVariables replacement
  undefinedVars = filter (\t -> not $ elem t patternVars) replaceVars
  undefinedErr  = unlines [ "The rule cannot be applied because the following"
                          , "pattern variables are used in the replacement but"
                          , "not defined within the pattern:"
                          , Utils.mkCSlist $ 
                            map QualifiedName.getOrigName undefinedVars
                          ]
  

  -- We return a 'Maybe ParsedComponent', if we cannot apply
  -- the rule to the given component then we return 'Nothing'
  -- This also allows us to say whether we have applied a rule
  -- which should only be applied a once (or a given number).
  applyRule :: ParsedComponent -> MainControl ParsedModel
  applyRule component
    | Just env <- matchPattern component pattern      =
      -- If the pattern currently matches then no problem we can just
      -- blend in the replacement and we're done.
      blendReplacement env replacement
    | (Cooperation left actions right) <- component
    , leftResult <- applyRule left
    , rightResult <- applyRule right                 =
      -- In this case we already know that we don't currently
      -- match the pattern but either the left or the right
      -- might. NOTE: that we do not use the new right if the
      -- left matches the pattern.
      if MainControl.hasResult leftResult
         -- The left matches so we do not care about the right
         then do leftModel <- leftResult
                 let newComp = Cooperation (modelSystemEqn leftModel) 
                                           actions  
                                           right
                 return $ leftModel { modelSystemEqn = newComp }
         else if MainControl.hasResult rightResult
              -- Okay the left didn't match but the right does
              then do rightModel <- rightResult
                      let newComp = Cooperation left
                                                actions
                                                (modelSystemEqn rightModel)
                      return $ rightModel { modelSystemEqn = newComp }
              -- Okay neither has completed so we may just return an
              -- error. I'm concatenating the errors this way otherwise
              -- (if we said simply "fail Cooperation left right doesn't match")
              -- then we would always get the error that system equation
              -- doesn't match.
              else leftResult >> rightResult
    | (ProcessArray comp size mActions) <- component  =
       do newComp <- applyRule comp
          let array = ProcessArray (modelSystemEqn newComp) size mActions
          return $ newComp { modelSystemEqn = array }
    | otherwise                                       =
       -- This is the final catch all case which will include 
       -- component identifiers that haven't matched the pattern.
       -- Here we have nothing left to try so we return 'Nothing'
       fail "Pattern could not be matched"

  -- 'matchPattern' must return an environment since some of the
  -- component matched by the pattern may be reused in the
  -- replacement by means of a pattern variable. We return
  -- 'Nothing' to indicate that the pattern failed to match
  -- and Just Environment to indicate that it succeeded.
  -- Note that of course the returned environment may well
  -- be empty.
  matchPattern :: ParsedComponent -> Pattern -> Maybe Environment
  matchPattern component (VarPat ident)                     =
    -- Of course a pattern variable matches any component
    return $ addComponent emptyEnvironment ident component
  matchPattern (IdProcess compId) (IdentPat patId)
    | refersTo patId compId = return emptyEnvironment
    | otherwise             = fail "Wrong component name"
  matchPattern (Cooperation left  actions  right)
               (CoopPat     pLeft pActions pRight)          =
    -- Notice if the pattern fails to match here we do not
    -- descent into the left and right components to see if the
    -- pattern matches there, 'applyRule' will take care of that
    -- we simply state whether the pattern matches currently.
    do leftEnv  <- matchPattern left pLeft
       rightEnv <- matchPattern right pRight
       actEnv   <- matchActions actions pActions
       return $ addActionMap (unionEnvironments leftEnv rightEnv) actEnv
  matchPattern (ProcessArray comp    size   mActions)
               (ArrayPat     compPat sizePat mActionsPat)   =
    -- TODO: Currently we don't match the size but we should and
    -- will of course need to in order to implement size changing.
    do compEnv <- matchPattern comp compPat
       exprEnv <- matchExprPattern size sizePat
       let env = addExprMap compEnv exprEnv
       case (mActions, mActionsPat) of
         (Nothing, Nothing) -> return env
         (Just _, Nothing)  -> fail "process array actions don't match"
         (Nothing, Just _)  -> fail "process array actions don't match"
         (Just a1, Just a2) -> do actEnv <- matchActions (ActionSet a1) a2
                                  return $ addActionMap env actEnv
  -- So basically at this point we have determined that the pattern
  -- does not match
  matchPattern (IdProcess _)         _pattern = fail "comp doesn't match"
  matchPattern (Cooperation _ _ _ )  _pattern = fail "comp doesn't match"
  matchPattern (ProcessArray _ _ _)  _pattern = fail "comp doesn't match"
  matchPattern (Hiding _ _)          _pattern = fail "comp doesn't match"
  -- Now the errors, that is sequential components found in the
  -- system equation.
  matchPattern (PrefixComponent _ _) _pattern = error errorMsg
  matchPattern (ComponentSum _ _)    _pattern = error errorMsg
  matchPattern (CondBehaviour _ _)   _pattern = error errorMsg
  matchPattern (StopProcess)         _pattern = error errorMsg

  errorMsg = "matchPattern serious error: " ++
             "sequential component in system equation"

  -- Matching the set of actions, the same as 'matchPattern' but
  -- for a set of actions. 
  matchActions :: CooperationSet -> ActionsPattern -> Maybe ActionMap
  matchActions (ActionSet actions) (AnyActions ident pActions)
    | all (\t -> elem t actionNames) pActions = return mapping
    | otherwise                               = fail failMsg
    where
    mapping        = Map.singleton ident nonExplict
    failMsg        = "Explicit actions in pattern not matched"
    -- These are the action in 'actions' but not mentioned
    -- explicitly in the pattern and hence those that the
    -- pattern variable should be mapped to.
    nonExplict    = ActionSet $ filter (not . isExplicit) actions
    isExplicit :: ParsedAction -> Bool
    isExplicit action = elem (nameOfAction action) pActions
    actionNames   = map nameOfAction actions
  matchActions (ActionSet actions) (Actions pActions)
    | not $ equalSetLists actionNames pActions     = 
      fail "Actions do not match"
    | otherwise                                    = 
      return Map.empty
    where
    actionNames   = map nameOfAction actions
  -- So basically currently a wildcard can only be matched by a
  -- pattern variable without any explicit actions.
  matchActions (WildCard) (AnyActions ident [])    =
    return $ Map.singleton ident WildCard
  -- So this could technically match but for now we'll
  -- assume that the author of the rule meant explicitly
  -- those actions. Potentially we could do this but it
  -- is quite awkward and requires a process action map.
  matchActions WildCard (AnyActions _ident _acts)  =
    fail "Actions do not match"
  matchActions (WildCard) (Actions _pActions)      =
    fail "Actions do not match"
 
  -- A utility function which tests if two lists define
  -- the same set. We do not care if an item occurs multiple
  -- times. 
  equalSetLists :: Eq a => [a] -> [a] -> Bool
  equalSetLists l1 l2 =
    -- All of the elements of l1 are in l2
    (all (\e -> elem e l2) l1) &&
    -- And all of the elements of l2 are in l1
    (all (\e -> elem e l1) l2)

  -- The same as 'matchPattern' but for expressions.
  matchExprPattern :: RateExpr -> ExprPattern -> Maybe ExpressionMap
  matchExprPattern (Cconstant i) (ConstantPat j)
    | i == j    = return Map.empty
    | otherwise = fail "Pattern does not match"
  matchExprPattern (Creal d) (RealPattern e)
    | d == e    = return Map.empty
    | otherwise = fail "Pattern does not match"
  matchExprPattern expr (AnyExpr var) =
    return $ Map.singleton var expr
  -- So here we go a bit wrong, we must decide what to do with
  -- compound expressions. We should at least match Cadd with
  -- PlusPattern etc. However what about (3) with (2 + 1), not very
  -- useful I agree, but what about about (3) with (?a + 1) where
  -- ?a is mapped to 2. We should be able to write this.
  matchExprPattern _expr _pattern     = 
    fail "Pattern does not match"

  -- To make the replacement we must transform the parsed replacement
  -- into a parsed component. Note that currently we only support
  -- straightforward dropping into place but in time we should allow
  -- the pattern to match on some names and hence we need to replace
  -- those names in the replacement. That may mean that here we
  -- need to take in an environment.
  blendReplacement :: Environment -> Replacement -> MainControl ParsedModel
  blendReplacement env (VarPat ident)                            =
    do component <- newComp
       return $ Pepa.emptyModel component
    where
    newComp  = maybe failVal return $ lookupComp env ident
    failVal = fail failMsg
    failMsg = unwords [ "Not found:", QualifiedName.textual ident ]
  blendReplacement _env (IdentPat ident)                         = 
    return $ Pepa.emptyModel (IdProcess ident)
  blendReplacement env (CoopPat left actionsPattern right)       =
    do newLeft  <- blendReplacement env left
       newRight <- blendReplacement env right
       actions  <- blendActions env actionsPattern
       let combineF l r = Cooperation l actions r
       return $ Pepa.combineModels combineF newLeft newRight
  blendReplacement env (ArrayPat comp size Nothing)              =
    do newComp <- blendReplacement env comp
       newSize <- blendExpression env size
       let array = ProcessArray (modelSystemEqn newComp) newSize Nothing
       return $ newComp { modelSystemEqn = array }
  blendReplacement env (ArrayPat comp size (Just actPat))        =
    do newComp    <- blendReplacement env comp
       newSize    <- blendExpression env size
       coopSet    <- blendActions env actPat
       -- Instead we could probably just get the action set of the array
       -- component. However it would be better to allow it in GENERAL
       -- for the array syntax if we're going to allow it at all here.
       array      <- case coopSet of
                      ActionSet newActions -> 
                        return $ ProcessArray (modelSystemEqn newComp)
                                              newSize 
                                              (Just newActions)
                      WildCard             ->
                        fail failMsg
       return $ newComp { modelSystemEqn = array }
       where
       failMsg = "You cannot have a wildcard cooperation set in an array"
  blendReplacement env (PartEval compRep)                        =
    do smallModel <- blendReplacement env compRep
       -- We need to combine the model returned with the original
       -- model since the original model will have all the process
       -- definitions and this one not necessarily. We use 'const'
       -- as the combining function since we want the blended component
       -- to be the actual system equation that gets partially
       -- evaluated.
       let fullModel = Pepa.combineModels const smallModel pModel
       PartEval.partEvalPepaModel fullModel
       
  -- The same as 'blendReplacement' but for a set of actions.
  blendActions :: Environment -> ActionsPattern -> MainControl CooperationSet
  blendActions _env (Actions actions)          = 
    return $ ActionSet $ map Action actions
  blendActions env (AnyActions var pActions)   = 
    do coopSet <- maybe failVal return $ lookupActions env var
       case coopSet of
         -- This is kind of questionable, certainly if wildcard doesn't
         -- include pActions, then it is wrong, but then in that case you
         -- have made a broken PEPA model by attempting to cooperate on
         -- an activity you do not perform.
         WildCard       -> return WildCard
         ActionSet acts -> return $ ActionSet $ acts ++ (map Action pActions)
    where
    failVal = fail failMsg
    failMsg = unwords [ "Could not match action set" ]

  -- The same as 'blendReplacement' and 'blendActions' but for an
  -- expression usually occuring as the size portion of a process array
  blendExpression :: Environment -> ExprPattern -> MainControl RateExpr
  blendExpression env (AnyExpr var)            = 
    maybe failVal return $ lookupExpr env var
    where
    failVal = fail failMsg
    failMsg = unwords [ "No such variable: ", QualifiedName.textual var ]
  blendExpression _env (ConstantPat i)         = return $ Cconstant i
  blendExpression _env (RealPattern d)         = return $ Creal d
  blendExpression env (PlusPattern left right) = 
    do newLeft  <- blendExpression env left
       newRight <- blendExpression env right
       return $ Cadd newLeft newRight
  blendExpression env (MinusPattern left right) = 
    do newLeft  <- blendExpression env left
       newRight <- blendExpression env right
       return $ Csub newLeft newRight
  blendExpression env (MultPattern left right)  = 
    do newLeft  <- blendExpression env left
       newRight <- blendExpression env right
       return $ Cmult newLeft newRight
  
-- A utility function to test whether a component name within
-- a rule refers to a component name within the model.
refersTo :: ComponentName -> QualifiedName -> Bool
refersTo = QualifiedName.sameOrigName


{-
  The type 'Environment' captures what we return when a pattern
  matches a component. If the pattern contains a pattern variable
  (ie. something that matches anything) then we add whatever is
  in that position in the matched component to the environment.
  We need several maps because a pattern variable may match against
  different parts of the model.

  Notice that this also allows us to have the same pattern variable
  mapped more than once depending on where it occurs in the pattern
  and the replacement, you can for example write;
  "?P[?P][?P] ==> ?P[?P + 1][?P,a]"
  although we don't really advertise this fact as it seems not to be
  particularly robust.
-}
data Environment = 
  Environment { envActions     :: ActionMap
              , envComponents  :: ComponentMap
              , envExpressions :: ExpressionMap
              }
-- The types of maps included in an environment
type ActionMap     = Map PatternVariable CooperationSet
type ComponentMap  = Map PatternVariable ParsedComponent
type ExpressionMap = Map PatternVariable RateExpr

{- The empty environment what is returned when the pattern
   matches completely and no pattern variables are used.
-}
emptyEnvironment :: Environment
emptyEnvironment = Environment { envActions     = Map.empty
                               , envComponents  = Map.empty
                               , envExpressions = Map.empty
                               }

-- Lookup a pattern variable which we expect to be mapped to a list
-- of actions.
lookupActions :: Environment -> PatternVariable -> Maybe CooperationSet
lookupActions env var = Map.lookup var $ envActions env

-- Lookup a pattern variable which we expect to be mapped to an expression
-- because it has occurred in an expression place in the replacement, for
-- example the size of an array.
lookupExpr :: Environment -> PatternVariable -> Maybe RateExpr
lookupExpr env var = Map.lookup var $ envExpressions env

-- Lookup a pattern variable which we expect to be mapped to a component
lookupComp :: Environment -> PatternVariable -> Maybe ParsedComponent
lookupComp env var = Map.lookup var $ envComponents env


-- Unify together two environments, to be honest we should really check
-- that any pattern variable is not defined more than once at the same
-- type. As mentioned above we can define the same pattern variable to
-- map against say a component and a set of actions, but we do not wish
-- to allow for example: "?P || ?P" as a pattern as it's not clear what
-- we should do at this point.
unionEnvironments :: Environment -> Environment -> Environment
unionEnvironments e1 e2 =
  Environment { envActions     = Map.union (envActions e1)
                                           (envActions e2)
              , envComponents  = Map.union (envComponents e1)
                                           (envComponents e2)
              , envExpressions = Map.union (envExpressions e1)
                                           (envExpressions e2)
              }

-- Having made an action map, add it to the given environment
-- As above we should make sure we are not overridding any
-- pattern variables already mapped to an action.
addActionMap :: Environment -> ActionMap -> Environment
addActionMap env am = env { envActions = Map.union (envActions env) am }

-- Having made an expression mapping add it to the given environement
-- Similarly we should not allow ourselves to overide and existing
-- mapping from a pattern variable to an expression.
addExprMap :: Environment -> ExpressionMap -> Environment
addExprMap env em = env { envExpressions = Map.union (envExpressions env) em }

-- Add the given component to the given environment.
-- Once again we should check that no mapping from the given pattern
-- variable to a component already exists.
addComponent :: Environment -> PatternVariable 
             -> ParsedComponent -> Environment
addComponent env pvar comp =
  env { envComponents = Map.insert pvar comp $ envComponents env }