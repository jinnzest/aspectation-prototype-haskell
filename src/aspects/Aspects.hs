module Aspects
  ( generateMissingDirectives
  ) where

import Control.Lens (view)
import Data.Map (Map, empty, insert, lookup, union)
import Data.Text (Text)
import Prelude as P (Maybe(Just, Nothing), ($), (.), concat, foldl, map)

import AlgorithmicComplexityDirectives (defaultAlgorithmicComplexityDirective)
import AspectsData as Asp (Directives)
import AspectsRegister
  ( DirectivesRegister(MkDirectivesRegister)
  , _algorithmicComplexityDirectives
  , _completeAbilityDirectives
  , _functionDomainDirectives
  , _lazinessDirectives
  , _memoryComplexityDirectives
  , _resourcesManagementDirectives
  , _sideEffectDirectives
  , _whenToRunDirectives
  )
import CompleteAbilityDirectives (defaultCompleteAbilityDirective)
import Errors (Errors)
import LazinessDirectives (defaultLazinessDirective)
import MemoryComplexityDirectives (defaultMemoryComplexityDirective)
import SemanticTree as Sem (Function(fSignature), SemanticTree(MkSemanticTree), FunctionSignature, extractFunctions)
import SideEffectDirectives (defaultSideEffectDirective)
import SyntaxToSemantic (semSignatureToNameArgs)
import WhenToRunDirectives (defaultWhenToRunDirective)

generateMissingDirectives :: SemanticTree -> (Function -> a) -> Directives a -> Directives a
generateMissingDirectives coreTree defaultDirective directives =
  let
    functions   = extractFunctions coreTree
    enrichedMap = foldl
      (\acc f ->
        let fs = fSignature f
        in
          case lookup fs directives of
            Nothing -> insert fs (defaultDirective f) acc
            Just v  -> insert fs v acc
      )
      empty
      functions
  in union enrichedMap directives
