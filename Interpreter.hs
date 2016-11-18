module Interpreter where

import Parser (Expression(..))
import Environment (Value(..), Environment)
import qualified Data.Map.Strict as Map

interpret :: Environment -> Expression -> Either String Value
interpret _ (StrExp s) = Right $ StrVal s
interpret _ (IntExp n) = Right $ IntVal n
interpret env (SymExp i) = case Map.lookup i env of
    Just v -> Right v
    Nothing -> Left $ "Reference to undefined identifier: '" ++ i ++ "'"
interpret _ (ListExp []) = Left "Application: no function given"
interpret env (ListExp (e:es)) = case interpret env e of
    Left err -> Left err
    Right (FunVal env' ps body) -> env'' >>= flip interpret body where
        env'' = flip Map.union (Map.union env' env) <$> args
        args = if length ps == length es
            then Map.fromList . zip ps <$> mapM (interpret env) es
            else Left "Argument arity mismatch"
    Right (Builtin _ f') -> mapM (interpret env) es >>= f'
    Right Lambda -> interpretLambda env es
    Right v -> Left $ "Application: not a function: '" ++ show v ++ "'"

interpretLambda :: Environment -> [Expression] -> Either String Value
interpretLambda env [ListExp ps,body] = case mapM extractSym ps of
    Just ps' -> Right $ FunVal env ps' body
    Nothing -> Left "Lambda: syntax error; first argument must be list of identifiers"
interpretLambda _ [_,_] = Left "Lambda: syntax error; first argument must be list of identifiers"
interpretLambda _ es = Left $ "Lambda: syntax error; expected two arguments, found " ++ show (length es)

extractSym :: Expression -> Maybe String
extractSym (SymExp s) = Just s
extractSym _ = Nothing
