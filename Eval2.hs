module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> case runStateError m s of
                                Left e -> Left e
                                Right (a :!: s') -> runStateError (f a) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError (\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> lookfor' v s)
    where lookfor' v s = case M.lookup v s of
                           Nothing -> Left UndefVar
                           Just x -> Right (x :!: s)
  update v i = StateError (\s -> Right (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case (runStateError (stepCommStar p) initEnv) of
           Left e -> Left e
           Right (a :!: s) -> Right s

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip                 = return Skip
stepComm (Let v e)            = letComm v e
stepComm (Seq c1 c2)          = seqComm c1 c2
stepComm (IfThen b c)         = ifThenElseComm b c Skip
stepComm r@(Repeat b c)       = return (Seq c (IfThenElse b r Skip)) 
stepComm (IfThenElse b c1 c2) = ifThenElseComm b c1 c2

letComm :: (MonadState m, MonadError m) => Variable -> Exp Int -> m Comm
letComm v e = do e' <- evalExp e
                 update v e'
                 return Skip

seqComm :: (MonadState m, MonadError m) => Comm -> Comm -> m Comm
seqComm Skip c2 = return c2
seqComm c1   c2 = do c1' <- stepComm c1
                     return (Seq c1' c2)

ifThenElseComm :: (MonadState m, MonadError m) => (Exp Bool) -> Comm -> Comm -> m Comm
ifThenElseComm b c1 c2 = do b' <- evalExp b
                            return (if b' then c1 else c2)

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Const n) = return n
evalExp (Var v) = lookfor v
evalExp (Not e) = do e' <- evalExp e
                     return (not e')
evalExp (UMinus e) = do e' <- evalExp e
                        return (-e')
evalExp (Plus e1 e2) = auxEvalExp e1 e2 (+)
evalExp (Minus e1 e2) = auxEvalExp e1 e2 (-)
evalExp (Times e1 e2) = auxEvalExp e1 e2 (*)
evalExp (Div e1 e2) = auxEvalDiv e1 e2
evalExp (Lt e1 e2) = auxEvalExp e1 e2 (<)
evalExp (Gt e1 e2) = auxEvalExp e1 e2 (>)
evalExp (And e1 e2) = auxEvalExp e1 e2 (&&)
evalExp (Or e1 e2) = auxEvalExp e1 e2 (||)
evalExp (Eq e1 e2) = auxEvalExp e1 e2 (==)
evalExp (NEq e1 e2) = auxEvalExp e1 e2 (/=)

-- Función auxiliar para evaluar expresiones binarias
auxEvalExp :: (MonadState m, MonadError m) =>  Exp a -> Exp a -> (a -> a -> b) -> m b
auxEvalExp e1 e2 f = do e1' <- evalExp e1
                        e2' <- evalExp e2
                        return (f e1' e2')

auxEvalDiv :: (MonadState m, MonadError m) =>  Exp Int -> Exp Int -> m Int
auxEvalDiv e1 e2 = do n2 <- evalExp e2
                      if n2 == 0 
                      then throw DivByZero 
                      else (do n1 <- evalExp e1
                               return (div n1 n2)) 
