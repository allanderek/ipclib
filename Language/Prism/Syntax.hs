{-|
   A suite of data types describing the abstract syntax of Prism models.
-}
module Language.Prism.Syntax
   ( PrismModelFile      ( .. )
   , PrismModel          ( .. )


   , PrismIdentifier
   , PrismModelKind      ( .. )
   , PrismModule         ( .. )
   , PrismVariable       ( .. )
   , PrismTransition     ( .. )
   , PrismCondition --   ( .. )
   , PrismAction    --   ( .. )
   , PrismRate
   )
where

{- Imported Standard Libraries -}
{- Imported Local Libraries -}
import Language.Pepa.QualifiedName
   ( QualifiedName )
import Language.Hydra.Syntax
   ( DNAMcondition
   , DNAMaction
   , DNAMspeed
   )
{- End of Imports -}

-- | The type of identifiers used within Prism models
type PrismIdentifier = QualifiedName

{-| The type of a prism model file -}
data PrismModelFile = PrismModelFile PrismModel


{-| The type of a Prism Model -}
data PrismModel = PrismModel PrismModelKind [ PrismModule ]


{-| The type of the model kind specification in prism -}
data PrismModelKind = PrismMdm
                    | PrismDtmc
                    | PrismCtmc


{-| The type of a prism module -}
data PrismModule = PrismModule PrismIdentifier
                               [ PrismVariable ] 
                               [ PrismTransition ]

{-| The type of a prism variable -}
data PrismVariable = PrismVariable { prismVarname :: PrismIdentifier
                                   , prismVarLow  :: Int
                                   , prismVarHigh :: Int
                                   , prismVarInit :: Int
                                   }

{-| The type of prism transitions -}
data PrismTransition =
   PrismTransition { prismTransGuard   :: [ PrismCondition ]
                   , prismTransActions :: [ PrismAction ]
                   , prismTransRate    :: PrismRate
                   }

{-| The type of prism guards on prism transitions -}
type PrismCondition = DNAMcondition

{-| The type of prism actions within prism transitions -}
type PrismAction = DNAMaction

{-| The type of a rate in prism -}
type PrismRate = DNAMspeed