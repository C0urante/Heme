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
interpret _ (AppExp []) = Left "Non-quoted empty list"
interpret env (AppExp (f:as)) = case interpret env f of
    Left err -> Left err
    Right (FunVal env' ps body) -> env'' >>= flip interpret body where
        env'' = flip Map.union (Map.union env' env) <$> args
        args = if length ps == length as
            then Map.fromList . zip ps <$> mapM (interpret env) as
            else Left "Argument arity mismatch"
    Right (Builtin _ f') -> mapM (interpret env) as >>= f'
    Right v -> Left $ "Not a function: '" ++ show v ++ "'"
interpret env (LambdaExp ps b) = Right $ FunVal env ps b
