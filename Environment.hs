module Environment where

import Parser (Expression)
import qualified Data.Map.Strict as Map (Map, fromList)
import Data.Char (toLower)

data Value =
    BoolVal Bool | StrVal String | IntVal Integer | ListVal [Value] |
    FunVal Environment FunParams Expression | Builtin String ([Value] -> Either String Value) |
    Define | Lambda | Apply | Let | If | Cond | And | Or | Void

data FunParams = FunParams [String] | VarParams String
    deriving Eq

instance Show FunParams where
    show (FunParams ps) = "(" ++ unwords ps ++ ")"
    show (VarParams p) = p

type Environment = Map.Map String Value

instance Eq Value where
    (BoolVal b) == (BoolVal b') = b == b'
    (StrVal s) == (StrVal s') = s == s'
    (IntVal n) == (IntVal n') = n == n'
    (ListVal l) == (ListVal l') = l == l'
    (FunVal env p body) == (FunVal env' p' body') = env == env' && p == p' && body == body'
    (Builtin i _) == (Builtin i' _) = i == i'
    Define == Define = True
    Lambda == Lambda = True
    Apply == Apply = True
    Let == Let = True
    If == If = True
    Cond == Cond = True
    And == And = True
    Or == Or = True
    Void == Void = True
    _ == _ = False

instance Show Value where
    show (BoolVal b) = map toLower $ show b
    show (StrVal s) = show s
    show (IntVal n) = show n
    show (ListVal l) = "(" ++ unwords (map show l) ++ ")"
    show (FunVal _ p b) = "(lambda " ++ show p ++ " " ++ show b ++ ")"
    show (Builtin i _) = "<builtin function '" ++ i ++ "'>"
    show Define = "<define>"
    show Lambda = "<lambda>"
    show Apply = "<apply>"
    show Let = "<let>"
    show If = "<if>"
    show And = "<and>"
    show Or = "<or>"
    show Cond = "<cond>"
    show Void = "<void>"

defaultEnv :: Environment
defaultEnv = Map.fromList [
        ("error", Builtin "error" errorFun),
        ("+",  Builtin "+"  $ arithFun 0 (+)),
        ("-",  Builtin "-"  $ arithFun 0 (-)),
        ("*",  Builtin "*"  $ arithFun 1 (*)),
        ("/",  Builtin "/"  $ arithFun 1 div),
        ("%",  Builtin "%"  $ arithFun 1 mod),
        ("<",  Builtin "<"  $ compFun (<)),
        ("<=", Builtin "<=" $ compFun (<=)),
        ("=",  Builtin "="  $ compFun (==)),
        ("!=", Builtin "!=" $ compFun (/=)),
        (">=", Builtin ">=" $ compFun (>=)),
        (">",  Builtin ">"  $ compFun (>)),
        ("equal?", Builtin "equal?" eqFun),
        ("boolean?", Builtin "boolean?" $ typePred isBool),
        ("string?", Builtin "string?" $ typePred isStr),
        ("number?", Builtin "number?" $ typePred isInt),
        ("list?", Builtin "list?" $ typePred isList),
        ("function?", Builtin "function?" $ typePred isFun),
        ("builtin?", Builtin "builtin?" $ typePred isBuiltin),
        ("list", Builtin "list" (Right . ListVal)),
        ("car", Builtin "car" carFun),
        ("cdr", Builtin "cdr" cdrFun),
        ("cons", Builtin "cons" consFun),
        ("length", Builtin "length" lengthFun),
        ("true", BoolVal True),
        ("false", BoolVal False),
        ("define", Define),
        ("lambda", Lambda),
        ("λ", Lambda),
        ("apply", Apply),
        ("let", Let),
        ("if", If),
        ("cond", Cond),
        ("and", And),
        ("or", Or),
        ("void", Void)
    ]

errorFun :: [Value] -> Either String Value
errorFun [v] = Left $ show v
errorFun vs = argCountError "1" vs

arithFun :: Integer -> (Integer -> Integer -> Integer) -> [Value] -> Either String Value
arithFun base _ [] = Right $ IntVal base
arithFun base op [v1] = IntVal <$> (op base <$> ensureIntVal v1)
arithFun _ op (v1:vs) = IntVal <$> (foldl op <$> ensureIntVal v1 <*> mapM ensureIntVal vs)

compFun :: (Integer -> Integer -> Bool) -> [Value] -> Either String Value
compFun op (v1:v2:vs) = BoolVal <$> helper (v1:v2:vs) where
    helper [] = Right True
    helper [_] = Right True
    helper (l:r:vs') = case op <$> ensureIntVal l <*> ensureIntVal r of
        (Left err) -> Left err
        (Right False) -> Right False
        (Right True) -> helper (r:vs')
compFun _ vs = argCountError "at least 2" vs

eqFun :: [Value] -> Either String Value
eqFun [v1, v2] = Right $ BoolVal $ v1 == v2
eqFun vs = argCountError "2" vs

typePred :: (Value -> Bool) -> [Value] -> Either String Value
typePred p [v] = Right $ BoolVal $ p v
typePred _ vs = argCountError "1" vs

isBool :: Value -> Bool
isBool (BoolVal _) = True
isBool _ = False

isStr :: Value -> Bool
isStr (StrVal _) = True
isStr _ = False

isInt :: Value -> Bool
isInt (IntVal _) = True
isInt _ = False

isList :: Value -> Bool
isList (ListVal _) = True
isList _ = False

isFun :: Value -> Bool
isFun (Builtin _ _) = True
isFun (FunVal _ _ _) = True
isFun _ = False

isBuiltin :: Value -> Bool
isBuiltin (Builtin _ _) = True
isBuiltin _ = False

carFun :: [Value] -> Either String Value
carFun [v] = head <$> (ensureListVal v >>= nullCheck)
carFun vs = argCountError "1" vs

cdrFun :: [Value] -> Either String Value
cdrFun [v] = (ListVal . tail) <$> (ensureListVal v >>= nullCheck)
cdrFun vs = argCountError "1" vs

consFun :: [Value] -> Either String Value
consFun [v,vs] = (ListVal . (v:)) <$> ensureListVal vs
consFun vs = argCountError "2" vs

lengthFun :: [Value] -> Either String Value
lengthFun [v] = (IntVal . toInteger . length) <$> ensureListVal v
lengthFun vs = argCountError "1" vs

nullCheck :: [Value] -> Either String [Value]
nullCheck [] = Left "Unexpected empty list"
nullCheck vs = Right vs

argCountError :: String -> [Value] -> Either String a
argCountError n vs =
    Left $ "Expected " ++ n ++ " arguments, found " ++ show (length vs) ++ " instead"

ensureBoolVal :: Value -> Either String Bool
ensureBoolVal (BoolVal b) = Right b
ensureBoolVal v = Left $ "Expected boolean, found '" ++ show v ++ "' instead"

ensureIntVal :: Value -> Either String Integer
ensureIntVal (IntVal n) = Right n
ensureIntVal v = Left $ "Expected integer, found '" ++ show v ++ "' instead"

ensureListVal :: Value -> Either String [Value]
ensureListVal (ListVal l) = Right l
ensureListVal v = Left $ "Expected list, found '" ++ show v ++ "' instead"
