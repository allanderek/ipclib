{-| 
    This module defines the stochastic probes which are used to define
    performance measures over pepa models.
-}
module Language.Pepa.Probes.Syntax
   ( ProbeR        ( .. )
   , ProbeDef
   , probeAlphabet
   , probeAlphabetList
   , probeLabelsList
   , firstNames

   -- * Probe analysis functions
   , containsNeedlessRep
   , isReplicatorProbe
   , endsInReplicatorProbe

   -- * Printing of probes
   , hprintProbeDef
   , pprintProbeDef
   )
where

{- Imported Standard Libraries -}
import qualified Data.Set as Set
import Data.Set
   ( Set )
import qualified Text.PrettyPrint.HughesPJ as Pretty
import Text.PrettyPrint.HughesPJ
  ( Doc )
{- Imported External Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.Syntax
  ( ActionIdentifier
  , ParsedComponentId
  )
import Language.Pepa.Compile.States
  ( StateCondition )
import qualified Language.Pepa.Print as PepaPrint
{- End of Imports -}


{-| 
   A probe definition is simply a top level (of the grammar)
   stochastic probe.
-}
type ProbeDef = (Maybe ParsedComponentId, ProbeR)

{-|
  A stochastic probe has the grammar:
  @
  R ::= a | T,T | (S)
  S ::= T | T|S
  T ::= R | R{n} | R{m,n} | R+ | R* | R? | R\act
  a ::= act | act:stop | act:start
  @
  
  The following data type definitions follow this specification
  pretty closely, although we greatly simplify this because
  we don't need the different levels of probes they can all be
  defined as one (a T can be an R anyway so we lose nothing in
  the way of type checking here, well that's not quite true the
  parser may benefit from defining ProbeR and ProbeS and ProbeT
  types, but not *that* much).
-}
data ProbeR = 
     SPact    ActionIdentifier        -- ^ Simple action\/activity observation
   | SPlabel  ActionIdentifier        -- ^  \: label 
   | SPcond   StateCondition ProbeR   -- ^  \{cexp\}R
   | SPseq    ProbeR ProbeR           -- ^  R1, R2 
   | SPchoice ProbeR ProbeR           -- ^  (R1 | R2) 
   | SPiter   ProbeR Int              -- ^  R{n} 
   | SPbit    ProbeR (Int, Int)       -- ^  R{m,n} 
   | SPoom    ProbeR                  -- ^  R+ 
   | SPzom    ProbeR                  -- ^  R* 
   | SPzoo    ProbeR                  -- ^  R? 
   | SPwo     ProbeR ActionIdentifier -- ^  R \/ act
   deriving (Show, Read)


{-|
  The alphabet of a probe returned as a list
-}
probeAlphabetList :: ProbeR -> [ ActionIdentifier ]
probeAlphabetList = Set.toList . probeAlphabet

{-|
  The alphabet of a probe is the actions over which it acts.
-}
probeAlphabet :: ProbeR -> Set ActionIdentifier
probeAlphabet (SPact act)      = Set.singleton act
probeAlphabet (SPlabel _)      = Set.empty
probeAlphabet (SPchoice r1 r2) = Set.union (probeAlphabet r1) 
                                           (probeAlphabet r2)
probeAlphabet (SPseq    r1 r2) = Set.union (probeAlphabet r1) 
                                           (probeAlphabet r2)
probeAlphabet (SPiter r1 _)    = probeAlphabet r1
probeAlphabet (SPbit r1 _)     = probeAlphabet r1
probeAlphabet (SPoom r1)       = probeAlphabet r1
probeAlphabet (SPzom r1)       = probeAlphabet r1
probeAlphabet (SPzoo r1)       = probeAlphabet r1
probeAlphabet (SPwo  r1 act)   = Set.insert act $ probeAlphabet r1
probeAlphabet (SPcond _c r)    = probeAlphabet r


{-|
  Returns as a list the labels of a probe.
-}
probeLabelsList :: ProbeR -> [ ActionIdentifier ]
probeLabelsList = Set.toList . probeLabels

{-| 
  Returns the set of probe labels executed by a probe.
-}
probeLabels :: ProbeR -> Set ActionIdentifier
probeLabels (SPlabel l)      = Set.singleton l
probeLabels (SPact _)        = Set.empty
probeLabels (SPchoice r1 r2) = Set.union (probeLabels r1) (probeLabels r2)
probeLabels (SPseq r1 r2)    = Set.union (probeLabels r1) (probeLabels r2)
probeLabels (SPiter r1 _)    = probeLabels r1
probeLabels (SPbit r1 _)     = probeLabels r1
probeLabels (SPoom r1)       = probeLabels r1
probeLabels (SPzom r1)       = probeLabels r1
probeLabels (SPzoo r1)       = probeLabels r1
probeLabels (SPwo r1 _)      = probeLabels r1
probeLabels (SPcond _c r)    = probeLabels r


{-|
    Computes the set of actions names which are explicitly 
    used at the beginning of a stochastic probe.
-}
firstNames :: ProbeR -> Set ActionIdentifier 
firstNames (SPact act)                  = Set.singleton act
firstNames (SPlabel label)              = Set.singleton label
firstNames (SPseq r1 r2)
   | canBeEmptySeq r1 = Set.union (firstNames r1) (firstNames r2)
   | otherwise        = firstNames r1
firstNames (SPchoice r1 r2)             = Set.union (firstNames r1) 
                                                    (firstNames r2)
firstNames (SPiter r1 _)                = firstNames r1
firstNames (SPbit r1 _)                 = firstNames r1
firstNames (SPzoo r1)                   = firstNames r1
firstNames (SPoom r1)                   = firstNames r1
firstNames (SPzom r1)                   = firstNames r1
firstNames (SPwo r1 _)                  = firstNames r1
firstNames (SPcond _c r)                = firstNames r

{-|
   Returns true if the given probe may be the empty sequence
-}
canBeEmptySeq :: ProbeR -> Bool
canBeEmptySeq (SPzom _)        = True
canBeEmptySeq (SPzoo _)        = True
canBeEmptySeq (SPiter _ 0)     = True
canBeEmptySeq (SPbit _ (0, _)) = True
canBeEmptySeq (SPbit _ (_, 0)) = True
canBeEmptySeq (SPchoice r1 r2) = (canBeEmptySeq r1) || (canBeEmptySeq r2)
canBeEmptySeq (SPseq r1 r2)    = (canBeEmptySeq r1) && (canBeEmptySeq r2)
canBeEmptySeq (SPwo r1 _)      = canBeEmptySeq r1
canBeEmptySeq _                = False


{-|
   Returns true if the probe is determined to contain a sub-probe
   which may perform an immediate communication directly after
   a replicator probe (such that the choice makes no sense).
-}
containsNeedlessRep :: ProbeR -> Bool
containsNeedlessRep (SPlabel _)           = False
containsNeedlessRep (SPact _)             = False
-- The important case
containsNeedlessRep (SPseq l r)           =
   -- We have a needless loop HERE
   (endsInReplicatorProbe l && beginsWithLabel r)
   -- Or one is contained within either of the subprobes
   || containsNeedlessRep l || containsNeedlessRep r
containsNeedlessRep (SPchoice l r)        =
   containsNeedlessRep l || containsNeedlessRep r
containsNeedlessRep (SPiter r _)          =
   containsNeedlessRep r
containsNeedlessRep (SPbit r _)           =
   containsNeedlessRep r
containsNeedlessRep (SPoom r)             =
   containsNeedlessRep r
containsNeedlessRep (SPzom r)             =
   containsNeedlessRep r
containsNeedlessRep (SPzoo r)             =
   containsNeedlessRep r
containsNeedlessRep (SPwo r _)            =
   containsNeedlessRep r
containsNeedlessRep (SPcond _c r)         =
   containsNeedlessRep r

{-| This returns true if the probe is a replicator probe.
   You almost never wish to use this on its own though.
   For example @(a* | b)@ returns @False@ even though the
   left hand is a replicator.
   So you would normally want a function such as @maybeReplicator@
   or 'endsInReplicatorProbe'
-}
isReplicatorProbe :: ProbeR -> Bool
isReplicatorProbe (SPact _)      = False
isReplicatorProbe (SPlabel  _ )  = False
isReplicatorProbe (SPseq _ _)    = False
isReplicatorProbe (SPchoice _ _) = False
isReplicatorProbe (SPiter _ _)   = True
isReplicatorProbe (SPbit  _ _)   = True
isReplicatorProbe (SPoom  _ )    = True
isReplicatorProbe (SPzom  _ )    = True
isReplicatorProbe (SPzoo  _ )    = True
isReplicatorProbe (SPwo r _ )    = isReplicatorProbe r
isReplicatorProbe (SPcond _ r)   = isReplicatorProbe r

{-|
   Returns true if this probe may end in a replicator probe.
-}
endsInReplicatorProbe :: ProbeR -> Bool
endsInReplicatorProbe (SPseq _ r)    = endsInReplicatorProbe r
endsInReplicatorProbe (SPchoice l r) =
   endsInReplicatorProbe l || endsInReplicatorProbe r
endsInReplicatorProbe (SPact _)      = False
endsInReplicatorProbe (SPlabel _)    = False
endsInReplicatorProbe (SPiter _ _)   = True
endsInReplicatorProbe (SPbit  _ _)   = True
endsInReplicatorProbe (SPoom  _ )    = True
endsInReplicatorProbe (SPzom  _ )    = True
endsInReplicatorProbe (SPzoo  _ )    = True
endsInReplicatorProbe (SPwo r _ )    = endsInReplicatorProbe r
endsInReplicatorProbe (SPcond _ r)   = endsInReplicatorProbe r

{-|
   Returns true if this probe begins with a label
-}
beginsWithLabel :: ProbeR -> Bool
beginsWithLabel (SPlabel _)    = True
beginsWithLabel (SPseq l _)    = beginsWithLabel l
beginsWithLabel (SPchoice l r) =
   beginsWithLabel l || beginsWithLabel r
beginsWithLabel (SPact _)      = False
beginsWithLabel (SPiter r _)   = beginsWithLabel r
beginsWithLabel (SPbit  r _)   = beginsWithLabel r
beginsWithLabel (SPoom  r )    = beginsWithLabel r
beginsWithLabel (SPzom  r )    = beginsWithLabel r
beginsWithLabel (SPzoo  r )    = beginsWithLabel r
beginsWithLabel (SPwo r _ )    = beginsWithLabel r
-- I think this is correct to say false here because
-- we use this to detect whether or not a loop is
-- needless. Consider ((a,b)*{P == 0}:go) if P <> 0
-- then we may observe the 'a' and therefore have to
-- wait to observe a 'b' so the choice caused by the
-- loop is not needless.
beginsWithLabel (SPcond _ _r)  = False

{- Printing out probe definitions -}
hprintProbeDef :: ProbeDef -> String
hprintProbeDef =
  Pretty.render . pprintProbeDef

pprintProbeDef :: ProbeDef -> Doc
pprintProbeDef (Just p, probe)  =
  Pretty.hcat [ pprintName p
              , Pretty.text "::"
              , pprintProbe probe
              ]
pprintProbeDef (Nothing, probe) =
  pprintProbe probe

pprintProbe :: ProbeR -> Doc
pprintProbe (SPact a)             = pprintName a
pprintProbe (SPlabel label)       = Pretty.hcat [ Pretty.text ":"
                                                , pprintName label
                                                ]
-- Labels are parsed as a sequence of a probe plus a label so we
-- must be careful NOT to print a comma between the left and the right
-- whenever the right hand side is label. So this case is really just
-- an amalgam of the one above it and the one below it.
pprintProbe (SPseq left 
              (SPlabel label))    = Pretty.hcat [ pprintProbe left
                                                , Pretty.text ":"
                                                , pprintName label
                                                ]
pprintProbe (SPseq left right)    = Pretty.hcat [ pprintProbe left
                                                , Pretty.comma
                                                , pprintProbe right
                                                ]
pprintProbe (SPchoice left right) = Pretty.sep [ pprintProbe left
                                               , Pretty.text "|"
                                               , pprintProbe right
                                               ]
pprintProbe (SPiter probe copies) = 
  Pretty.hcat [ pprintProbe probe
              , Pretty.braces $ Pretty.int copies
              ]
pprintProbe (SPbit probe (m,n))   =
  Pretty.hcat [ pprintProbe probe
              , Pretty.braces $ Pretty.hcat [ Pretty.int m
                                            , Pretty.comma
                                            , Pretty.int n
                                            ]
              ]
pprintProbe (SPoom probe)         = 
  Pretty.hcat [ pprintProbe probe
              , Pretty.text "+"
              ]
pprintProbe (SPzom probe)         = 
  Pretty.hcat [ pprintProbe probe
              , Pretty.text "*"
              ]
pprintProbe (SPzoo probe)         = 
  Pretty.hcat [ pprintProbe probe
              , Pretty.text "?"
              ]
pprintProbe (SPwo probe action)   = 
  Pretty.hcat [ pprintProbe probe
              , Pretty.text "/"
              , pprintName action
              ]
pprintProbe (SPcond cond probe)   =
  Pretty.hcat [ Pretty.braces (Pretty.text $ PepaPrint.hprintRateExpr cond)
              , pprintProbe probe
              ]

pprintName :: ActionIdentifier -> Doc
pprintName = PepaPrint.actionToDoc