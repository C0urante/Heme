module Interpreter where

import Parser (Expression(..))
import Environment (Value(..), Environment)
import qualified Data.Map.Strict as Map

interpret :: Environment -> Expression -> Either String Value
interpret _ (StrExp s) = Right $ StrVal s
interpret _ (IntExp n) = Right $ IntVal n
interpret env (VarExp i) = case Map.lookup i env of
    Just v -> Right v
    Nothing -> Left $ "Reference to undefined identifier: '" ++ i ++ "'"
interpret _ (AppExp []) = Left "Application: no function given"
interpret env (AppExp (f:as)) = case interpret env f of
    Left err -> Left err
    Right (FunVal env' ps body) -> env'' >>= flip interpret body where
        env'' = flip Map.union (Map.union env' env) <$> args
        args = if length ps == length as
            then Map.fromList . zip ps <$> mapM (interpret env) as
            else Left "Argument arity mismatch"
    Right (Builtin _ f') -> mapM (interpret env) as >>= f'
    Right v -> Left $ "Application: not a function: '" ++ show v ++ "'"
interpret env (LambdaExp ps e) = Right $ FunVal env ps e
interpret env (LetExp bs body) = env' >>= flip interpret body where
    (is,vs) = unzip bs
    bindings = Map.fromList . zip is <$> mapM (interpret env) vs
    env' = flip Map.union env <$> bindings
interpret env (IfExp c t f) = case interpret env c of
    Left err -> Left err
    Right (BoolVal True) -> interpret env t
    Right (BoolVal False) -> interpret env f
    Right v -> Left $ "If: not a boolean: '" ++ show v ++ "'"
interpret _ (CondExp []) = Left "Cond expression failed to match any cases"
interpret env (CondExp ((b,v):cs)) = case interpret env b of
    Left err -> Left err
    Right (BoolVal True) -> interpret env v
    Right (BoolVal False) -> interpret env (CondExp cs)
    Right v' -> Left $ "Cond: not a boolean: '" ++ show v' ++ "'"
