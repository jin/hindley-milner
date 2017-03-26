module Infer where

import Syntax

data TypeInferenceError = TypeInferenceError
  deriving (Show, Eq)

data InferredType = Int | Bool | Arrow InferredType InferredType
  deriving (Show, Eq)

infer :: Expr -> Either TypeInferenceError InferredType
infer _ = Right Int 
