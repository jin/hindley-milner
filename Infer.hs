module Infer where

import Data.Maybe (fromMaybe)
import Data.Map (Map, map)
import qualified Data.Map as Map

import Syntax

newtype TypeInferenceError = TypeInferenceError String deriving Show

data InferredType = IntT
                  | BoolT
                  | StringT
                  | ArrowT InferredType InferredType
                  | TypeVar String
                    deriving (Show, Eq)

-- A type environment TypeEnv is a mapping from a program variable to an inferred type.
newtype TypeEnv = TypeEnv (Map String InferredType) deriving Show

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

addTypeBinding :: String -> InferredType -> TypeEnv -> TypeEnv
addTypeBinding var t (TypeEnv env) = TypeEnv (Map.insert var t env)

getType :: String -> TypeEnv -> InferredType
getType x (TypeEnv env) = fromMaybe raiseError $ Map.lookup x env
  where
    raiseError = error $ x ++ " is not defined in the environment."

-- Type substitutions
type TypeSubstMap = [InferredType -> InferredType]

emptySubst :: TypeSubstMap
emptySubst = []

addSubst :: InferredType -> InferredType -> TypeSubstMap -> TypeSubstMap
addSubst fromType toType substs = sub : substs
  where sub t = if t == fromType then toType else fromType

substType :: TypeSubstMap -> InferredType -> InferredType
substType substs fromType = foldr (\sub t -> sub t) fromType substs

substEnv :: TypeSubstMap -> TypeEnv -> TypeEnv
substEnv substs (TypeEnv env) = TypeEnv (foldr Data.Map.map env substs)

subst :: TypeSubstMap -> InferredType -> InferredType
subst _   IntT           = IntT
subst _   BoolT          = BoolT
subst _   StringT        = StringT
subst sub (ArrowT t1 t2) = ArrowT (subst sub t1) (subst sub t2)
subst sub v@(TypeVar _)  = substType sub v

unify :: InferredType -> InferredType -> TypeSubstMap
unify IntT           IntT             = emptySubst
unify BoolT          BoolT            = emptySubst
unify StringT        StringT          = emptySubst
unify (ArrowT t1 t2) (ArrowT t1' t2') = subst2 ++ subst1
  where
    subst1 = unify t1 t1'
    subst2 = unify (substType subst1 t2) (substType subst1 t2')
unify t v@(TypeVar _) = if v == t || not (occursIn v t)
                           then addSubst v t emptySubst
                           else emptySubst
unify v@(TypeVar _) t = if v == t || not (occursIn v t)
                           then addSubst v t emptySubst
                           else emptySubst
unify t1 t2           = error $ "Unable to unify types: " ++ show t1 ++ " and " ++ show t2

occursIn :: InferredType -> InferredType -> Bool
occursIn _ IntT           = False
occursIn _ BoolT          = False
occursIn _ StringT        = False
occursIn v (ArrowT t1 t2) = occursIn v t1 || occursIn v t2
occursIn v t@(TypeVar _)  = v == t

-- Algorithm W
infer :: TypeEnv -> Expr -> (InferredType, TypeSubstMap)
infer _   (I _)           = (IntT, emptySubst)
infer _   (S _)           = (StringT, emptySubst)
infer _   (B _)           = (BoolT, emptySubst)
infer env (Var x)         = (getType x env, emptySubst)
-- infer env (Fn y1 y2)      = undefined
-- infer env (Fun y1 y2 y3)  = undefined 
-- infer env (FApp y1 y2)    = undefined 
infer env (Cond e0 e1 e2) = (substType s4 $ substType s3 t2, s4 ++ s3 ++ s2 ++ s1)
  where
    (t0, s0) = infer env e0
    (t1, s1) = infer (substEnv s0 env) e1
    (t2, s2) = infer (substEnv s1 $ substEnv s0 env) e2
    s3       = unify (substType s2 $ substType s1 t0) BoolT
    s4       = unify (substType s3 t2) (substType s3 $ substType s2 t1)
infer env (Let x e1 e2)   = (t2, s2 ++ s1)
  where
    (t1, s1) = infer env e1
    (t2, s2) = infer (addTypeBinding x t1 env) e2
infer env (BinOp op e1 e2) = (opType op, s4 ++ s3 ++ s2 ++ s1) 
  where
    opType rator | rator `elem` [GTE, LTE, Equal] = BoolT
                 | rator `elem` [Add, Sub, Div, Mul] = IntT
                 | otherwise = error "Undefined operator"
    leftOperandType _ = IntT
    rightOperandType _ = IntT
    (t1, s1) = infer env e1
    (t2, s2) = infer (substEnv s1 env) e2
    s3 = unify (substType s2 t1) (leftOperandType op)
    s4 = unify (substType s3 t2) (rightOperandType op)
