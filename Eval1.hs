module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip                 = return Skip
stepComm (Let v e)            = letComm v e
stepComm (Seq c1 c2)          = seqComm c1 c2
stepComm (IfThen b c)         = ifThenElseComm b c Skip
stepComm r@(Repeat b c)       = return (Seq c (IfThenElse b Skip r)) 
stepComm (IfThenElse b c1 c2) = ifThenElseComm b c1 c2

letComm :: MonadState m => Variable -> Exp Int -> m Comm
letComm v e = do evalExp (EAssgn v e)
                 return Skip

seqComm :: MonadState m => Comm -> Comm -> m Comm
seqComm Skip c2 = return c2
seqComm c1   c2 = do c1' <- stepComm c1
                     return (Seq c1' c2)

ifThenElseComm :: MonadState m => (Exp Bool) -> Comm -> Comm -> m Comm
ifThenElseComm b c1 c2 = do b' <- evalExp b
                            return (if b' then c1 else c2)

-- Función auxiliar para evaluar expresiones binarias
--auxEvalExp :: Exp a -> Exp a -> (a -> a -> b) -> m b
auxEvalExp e1 e2 f = do e1' <- evalExp e1
                        e2' <- evalExp e2
                        return (f e1' e2')

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var v) = lookfor v
evalExp (UMinus e) = do e' <- evalExp e
                        return (-e')
evalExp (Plus e1 e2) = auxEvalExp e1 e2 (+)
evalExp (Minus e1 e2) = auxEvalExp e1 e2 (-)
evalExp (Times e1 e2) = auxEvalExp e1 e2 (*)
evalExp (Div e1 e2) = auxEvalExp e1 e2 (div)
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Not e) = do e' <- evalExp e
                     return (not e')
evalExp (Lt e1 e2) = auxEvalExp e1 e2 (<)
evalExp (Gt e1 e2) = auxEvalExp e1 e2 (>)
evalExp (And e1 e2) = auxEvalExp e1 e2 (&&)
evalExp (Or e1 e2) = auxEvalExp e1 e2 (||)
evalExp (Eq e1 e2) = auxEvalExp e1 e2 (==)
evalExp (NEq e1 e2) = auxEvalExp e1 e2 (/=)
evalExp (EAssgn v e) = do e' <- evalExp e
                          update v e'
                          return e'
--evalExp ESeq e1 e2 = 