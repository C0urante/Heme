module Environment where

import Parser (Expression)
import qualified Data.Map.Strict as Map (Map, fromList)
import Data.Char (toLower)

data Value =
    BoolVal Bool | StrVal String | IntVal Integer | ListVal [Value] |
    FunVal Environment [String] Expression | Builtin String ([Value] -> Either String Value) |
    Define | Lambda | Let | If | Cond | And | Or | Void

type Environment = Map.Map String Value

instance Eq Value where
    (BoolVal b) == (BoolVal b') = b == b'
    (StrVal s) == (StrVal s') = s == s'
    (IntVal n) == (IntVal n') = n == n'
    (ListVal l) == (ListVal l') = l == l'
    (FunVal env as body) == (FunVal env' as' body') = env == env' && as == as' && body == body'
    (Builtin i _) == (Builtin i' _) = i == i'
    Define == Define = True
    Lambda == Lambda = True
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
    show (FunVal _ _ _) = "<user-defined function>"
    show (Builtin i _) = "<builtin function '" ++ i ++ "'>"
    show Define = "<define>"
    show Lambda = "<lambda>"
    show Let = "<let>"
    show If = "<if>"
    show And = "<and>"
    show Or = "<or>"
    show Cond = "<cond>"
    show Void = "<void>"

defaultEnv :: Environment
defaultEnv = Map.fromList [
        ("+",  Builtin "+"  $ arithFun (+)),
        ("-",  Builtin "-"  $ arithFun (-)),
        ("*",  Builtin "*"  $ arithFun (*)),
        ("/",  Builtin "/"  $ arithFun div),
        ("%",  Builtin "%"  $ arithFun mod),
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
        ("true", BoolVal True),
        ("false", BoolVal False),
        ("define", Define),
        ("lambda", Lambda),
        ("λ", Lambda),
        ("let", Let),
        ("if", If),
        ("cond", Cond),
        ("and", And),
        ("or", Or),
        ("void", Void)
    ]

arithFun :: (Integer -> Integer -> Integer) -> [Value] -> Either String Value
arithFun op [v1, v2] = IntVal <$> (op <$> ensureIntVal v1 <*> ensureIntVal v2)
arithFun _ vs = argCountError 2 vs

compFun :: (Integer -> Integer -> Bool) -> [Value] -> Either String Value
compFun op [v1, v2] = BoolVal <$> (op <$> ensureIntVal v1 <*> ensureIntVal v2)
compFun _ vs = argCountError 2 vs

eqFun :: [Value] -> Either String Value
eqFun [v1, v2] = Right $ BoolVal $ v1 == v2
eqFun vs = argCountError 2 vs

typePred :: (Value -> Bool) -> [Value] -> Either String Value
typePred p [v] = Right $ BoolVal $ p v
typePred _ vs = argCountError 1 vs

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

notFun :: [Value] -> Either String Value
notFun [v] = (BoolVal . not) <$> ensureBoolVal v
notFun vs = argCountError 1 vs

carFun :: [Value] -> Either String Value
carFun [v] = head <$> (ensureListVal v >>= nullCheck)
carFun vs = argCountError 1 vs

cdrFun :: [Value] -> Either String Value
cdrFun [v] = (ListVal . tail) <$> (ensureListVal v >>= nullCheck)
cdrFun vs = argCountError 1 vs

consFun :: [Value] -> Either String Value
consFun [v,vs] = (ListVal . (v:)) <$> ensureListVal vs
consFun vs = argCountError 2 vs

nullCheck :: [Value] -> Either String [Value]
nullCheck [] = Left "Unexpected empty list"
nullCheck vs = Right vs

argCountError :: Int -> [Value] -> Either String a
argCountError n vs =
    Left $ "Expected " ++ show n ++ " arguments, found " ++ show (length vs) ++ " instead"

ensureBoolVal :: Value -> Either String Bool
ensureBoolVal (BoolVal b) = Right b
ensureBoolVal v = Left $ "Expected boolean, found '" ++ show v ++ "' instead"

ensureIntVal :: Value -> Either String Integer
ensureIntVal (IntVal n) = Right n
ensureIntVal v = Left $ "Expected integer, found '" ++ show v ++ "' instead"

ensureListVal :: Value -> Either String [Value]
ensureListVal (ListVal l) = Right l
ensureListVal v = Left $ "Expected list, found '" ++ show v ++ "' instead"
