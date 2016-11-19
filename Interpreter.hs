module Interpreter where

import Parser (Expression(..))
import Environment (Value(..), Environment)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

interpret :: Environment -> Expression -> Either String (Environment, Value)
interpret env (ListExp (e:es)) = case interpret' env e of
    Left err -> Left err
    Right Define -> flip (,) Void <$> interpretDefine env es
    Right f -> (,) env <$> interpretApplication env f es
interpret env e = (,) env <$> interpret' env e

interpret' :: Environment -> Expression -> Either String Value
interpret' _ (StrExp s) = Right $ StrVal s
interpret' _ (IntExp n) = Right $ IntVal n
interpret' env (SymExp i) = case Map.lookup i env of
    Just v -> Right v
    Nothing -> Left $ "Reference to undefined identifier: '" ++ i ++ "'"
interpret' _ (ListExp []) = Left "Application: no function given"
interpret' env (ListExp (e:es)) = case interpret' env e of
    Left err -> Left err
    Right f -> interpretApplication env f es

interpretApplication :: Environment -> Value -> [Expression] -> Either String Value
interpretApplication env Lambda es = interpretLambda env es
interpretApplication env Apply es = interpretApply env es
interpretApplication env Let es = interpretLet env es
interpretApplication env If es = interpretIf env es
interpretApplication env Cond es = interpretCond env es
interpretApplication env And es = interpretAnd env es
interpretApplication env Or es = interpretOr env es
interpretApplication _ Define _ = Left "Define: application error; only allowed as top-level expression"
interpretApplication env v es = mapM (interpret' env) es >>= interpretFunction env v

interpretFunction :: Environment -> Value -> [Value] -> Either String Value
interpretFunction env (FunVal env' ps body) args = env'' >>= flip interpret' body where
    env'' = flip Map.union (Map.union env' env) <$> args'
    args' = if length ps == length args
        then Right $ Map.fromList $ zip ps args
        else Left $ "Application: application error; expected " ++ show (length ps) ++ " arguments, found " ++ show (length args)
interpretFunction _ (Builtin _ f) args = f args
interpretFunction _ v _ = Left $ "Application: type error; expected function, found " ++ show v

interpretDefine :: Environment -> [Expression] -> Either String Environment
interpretDefine env [SymExp s,e] = flip (Map.insert s) env <$> interpret' env e
interpretDefine _ [_,_] = Left "Define: syntax error; first argument must be identifier"
interpretDefine _ es = Left $ "Define: syntax error; expected two arguments, found " ++ show (length es)

interpretLambda :: Environment -> [Expression] -> Either String Value
interpretLambda env [ListExp ps,body] = case mapM extractSym ps of
    Just ps' -> Right $ FunVal env ps' body
    Nothing -> Left "Lambda: syntax error; first argument must be list of identifiers"
interpretLambda _ [_,_] = Left "Lambda: syntax error; first argument must be list of identifiers"
interpretLambda _ es = Left $ "Lambda: syntax error; expected two arguments, found " ++ show (length es)

extractSym :: Expression -> Maybe String
extractSym (SymExp s) = Just s
extractSym _ = Nothing

interpretApply :: Environment -> [Expression] -> Either String Value
interpretApply env [f,l] = case (,) <$> interpret' env f <*> interpret' env l of
    Left err -> Left err
    Right (f',ListVal l') -> interpretFunction env f' l'
    Right (_,l') -> Left $ "Apply: type error; second argument must be list, found " ++ show l'
interpretApply _ es = Left $ "Apply: syntax error; expected two arguments, found " ++ show (length es)

interpretLet :: Environment -> [Expression] -> Either String Value
interpretLet env [ListExp bs,body] = case mapM extractLetPair bs of
    Nothing -> Left "Let: syntax error; first argument must be list of identifier, expression pairs"
    Just bs' -> env' >>= flip interpret' body where
        env' = flip Map.union env <$> bindings
        bindings = (Map.fromList . zip is) <$> mapM (interpret' env) vs
        (is,vs) = unzip bs'
interpretLet _ [_,_] = Left "Let: syntax error; first argument must be list of identifier, expression pairs"
interpretLet _ es = Left $ "Let: syntax error; expected two arguments, found " ++ show (length es)

extractLetPair :: Expression -> Maybe (String, Expression)
extractLetPair (ListExp [SymExp i,e]) = Just (i, e)
extractLetPair _ = Nothing

interpretIf :: Environment -> [Expression] -> Either String Value
interpretIf env [c,t,f] = case interpret' env c of
    Right (BoolVal b) -> interpret' env (if b then t else f)
    Right v -> Left $ "If: type error; first argument must be boolean, found " ++ show v
    Left err -> Left err
interpretIf _ es = Left $ "If: syntax error; expected two arguments, found " ++ show (length es)

interpretCond :: Environment -> [Expression] -> Either String Value
interpretCond env cs = case mapM extractCondPair cs of
    Nothing -> Left "Cond: syntax error; each argument must be an expression, expression pair"
    Just cs' -> fromMaybe err $ listToMaybe $ mapMaybe (evalCondPair env) cs' where
        err = Left "Cond: application error; none of the provided expression, expression pairs had a first expression that evaluated to true"

extractCondPair :: Expression -> Maybe (Expression, Expression)
extractCondPair (ListExp [e1,e2]) = Just (e1,e2)
extractCondPair _ = Nothing

evalCondPair :: Environment -> (Expression, Expression) -> Maybe (Either String Value)
evalCondPair env (b,v) = case interpret' env b of
    Left err -> Just $ Left err
    Right (BoolVal True) -> Just $ interpret' env v
    Right (BoolVal False) -> Nothing
    Right b' -> Just $ Left $ "Cond: type error; first expression in each pair must be boolean, found " ++ show b'

interpretAnd :: Environment -> [Expression] -> Either String Value
interpretAnd _ [] = Right $ BoolVal True
interpretAnd env [e] = interpret' env e
interpretAnd env (e:es) = case interpret' env e of
    Left err -> Left err
    Right (BoolVal False) -> Right $ BoolVal False
    Right _ -> interpretAnd env es

interpretOr :: Environment -> [Expression] -> Either String Value
interpretOr _ [] = Right $ BoolVal False
interpretOr env [e] = interpret' env e
interpretOr env (e:es) = case interpret' env e of
    Left err -> Left err
    Right (BoolVal False) -> interpretOr env es
    Right v -> Right v
