module Infer (
  inferExpr, 
  emptyEnv
  ) where

import           Control.Monad.State
import           Data.Maybe (fromMaybe)
import           Data.Map (Map, map)
import qualified Data.Map as Map

import Syntax

data InferredType = IntT
                  | BoolT
                  | StringT
                  | ArrowT InferredType InferredType
                  | TypeVar String
                    deriving Eq

instance Show InferredType where
  show IntT = "Int"
  show BoolT = "Bool"
  show StringT = "String"
  show (ArrowT t1 t2) = show t1 ++ " -> " ++ show t2
  show (TypeVar tv) = '\'' : tv

newtype TypeInferenceError = TypeInferenceError String deriving Show

newtype Unique = Unique { count :: Int }

-- Use a State monad to generate fresh variables 
-- https://github.com/sdiehl/write-you-a-haskell
type Infer a = (State Unique) a

initUnique :: Unique
initUnique = Unique { count = 0 }

runInfer :: Infer (InferredType, TypeSubstMap) -> InferredType
runInfer m = let (t, _) = evalState m initUnique in t

supply :: [String]
supply = [replicate cnt v | cnt <- [1..], v <- ['a'..'z']]

fresh :: Infer InferredType
fresh = do
  s <- get
  put s { count = count s + 1 }
  return $ TypeVar (supply !! count s)

-- A type environment TypeEnv is a mapping from a program variable to an inferred type.
newtype TypeEnv = TypeEnv (Map String InferredType) deriving Show

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

addTypeBinding :: String -> InferredType -> TypeEnv -> TypeEnv
addTypeBinding var t (TypeEnv env) = TypeEnv (Map.insert var t env)

getType :: String -> TypeEnv -> InferredType
getType x (TypeEnv env) = fromMaybe raiseError $ Map.lookup x env
  where raiseError = error $ x ++ " is not defined in the environment."

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

unify :: InferredType -> InferredType -> Infer TypeSubstMap
unify IntT           IntT             = return emptySubst
unify BoolT          BoolT            = return emptySubst
unify StringT        StringT          = return emptySubst
unify (ArrowT t1 t2) (ArrowT t1' t2') = do
  subst1 <- unify t1 t1'
  subst2 <- unify (substType subst1 t2) (substType subst1 t2')
  return $ subst2 ++ subst1
unify t v@(TypeVar _) = if v == t || not (occursIn v t)
                           then return $ addSubst v t emptySubst
                           else return emptySubst
unify v@(TypeVar _) t = if v == t || not (occursIn v t)
                           then return $ addSubst v t emptySubst
                           else return emptySubst
unify t1 t2 = error $ "Unable to unify types: " ++ show t1 ++ " and " ++ show t2

-- TODO(jin): verify definition of occurs
occursIn :: InferredType -> InferredType -> Bool
occursIn _ IntT           = False
occursIn _ BoolT          = False
occursIn _ StringT        = False
occursIn v (ArrowT t1 t2) = occursIn v t1 || occursIn v t2
occursIn v t@(TypeVar _)  = v == t

-- Algorithm W
infer :: TypeEnv -> Expr -> Infer (InferredType, TypeSubstMap)
infer _   (I _)     = return (IntT, emptySubst)
infer _   (S _)     = return (StringT, emptySubst)
infer _   (B _)     = return (BoolT, emptySubst)

infer env (Var x)   = return (getType x env, emptySubst)

infer env (Fn x e0) = do
  fVar     <- fresh
  (t0, s0) <- infer (addTypeBinding x fVar env) e0
  return (ArrowT (substType s0 fVar) t0, s0)

infer env (Fun f x e0) = do
  tDom       <- fresh
  tRange     <- fresh
  let newEnv = addTypeBinding f (ArrowT tDom tRange) $ addTypeBinding x tDom env
  (t0, s0)   <- infer newEnv e0
  s1         <- unify t0 (substType s0 tRange)
  return (ArrowT (substType s1 (substType s0 tDom)) (substType s1 t0), s1 ++ s0)

infer env (FApp e1 e2) = do
  (t1, s1) <- infer env e1
  (t2, s2) <- infer (substEnv s1 env) e2
  fVar     <- fresh
  s3       <- unify (substType s2 t1) (ArrowT t2 fVar)
  return (substType s3 fVar, s3 ++ s2 ++ s1)

infer env (Cond e0 e1 e2) = do
  (t0, s0) <- infer env e0
  (t1, s1) <- infer (substEnv s0 env) e1
  (t2, s2) <- infer (substEnv s1 $ substEnv s0 env) e2
  s3       <- unify (substType s2 $ substType s1 t0) BoolT
  s4       <- unify (substType s3 t2) (substType s3 $ substType s2 t1)
  return (substType s4 $ substType s3 t2, s4 ++ s3 ++ s2 ++ s1)

infer env (Let x e1 e2) = do
  (t1, s1) <- infer env e1
  (t2, s2) <- infer (addTypeBinding x t1 env) e2
  return (t2, s2 ++ s1)

infer env (BinOp op e1 e2) = do
  (t1, s1) <- infer env e1
  (t2, s2) <- infer (substEnv s1 env) e2
  s3       <- unify (substType s2 t1) (leftOperandType op)
  s4       <- unify (substType s3 t2) (rightOperandType op)
  return (opType op, s4 ++ s3 ++ s2 ++ s1)

opType :: Op -> InferredType
opType rator | rator `elem` [GTE, LTE, Equal] = BoolT
             | rator `elem` [Add, Sub, Div, Mul] = IntT
             | otherwise = error "Undefined operator"

leftOperandType :: t -> InferredType
leftOperandType _ = IntT

rightOperandType :: t -> InferredType
rightOperandType _ = IntT

inferExpr :: TypeEnv -> Expr -> InferredType
inferExpr env = runInfer . infer env
