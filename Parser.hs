module Parser where

import Tokenizer (Token(..), matchParen, TokenStream)

data Expression =
    VarExp String | StrExp String | IntExp Integer | AppExp [Expression] |
    LambdaExp [String] Expression | LetExp [(String, Expression)] Expression |
    IfExp Expression Expression Expression | CondExp [(Expression, Expression)]
    deriving (Eq, Show)

data Expression' =
    IDExp' String | StrExp' String | IntExp' Integer | ListExp' [Expression'] |
    Lambda | Let | If | Cond
    deriving (Eq, Show)

parse :: TokenStream -> (Either String Expression, TokenStream)
parse [] = (Left "Unexpected end of token stream", [])
parse ts = let (e', ts') = parse' ts in (e' >>= parseHelper, ts')

parseHelper :: Expression' -> Either String Expression
parseHelper (IDExp' i) = Right $ VarExp i
parseHelper (StrExp' s) = Right $ StrExp s
parseHelper (IntExp' n) = Right $ IntExp n
parseHelper (ListExp' (Lambda:es')) = parseLambda es'
parseHelper (ListExp' (Let:es')) = parseLet es'
parseHelper (ListExp' (If:es')) = parseIf es'
parseHelper (ListExp' (Cond:es')) = parseCond es'
parseHelper (ListExp' es') = AppExp <$> mapM parseHelper es'
parseHelper Lambda = Left "Unexpected use of lambda"
parseHelper Let = Left "Unexpected use of let"
parseHelper If = Left "Unexpected use of if"
parseHelper Cond = Left "Unexpected use of cond"

parseLambda :: [Expression'] -> Either String Expression
parseLambda [ListExp' l,body] = case mapM extractID l of
    Just ps -> LambdaExp ps <$> parseHelper body
    Nothing -> Left "Invalid lambda syntax; first argument must be list of identifiers"
parseLambda [_,_] = Left "Invalid lambda syntax; first argument must be list of identifiers"
parseLambda es' = Left $ "Invalid lambda syntax; expected two arguments, got " ++ show (length es')

parseLet :: [Expression'] -> Either String Expression
parseLet [ListExp' l,body] = case mapM extractLetPair l of
    Just p -> let (is, vs) = unzip p in
        LetExp <$> (zip is <$> mapM parseHelper vs) <*> parseHelper body
    Nothing -> Left "Invalid let syntax; first argument must be list of identifier, expression pairs"
parseLet [_,_] = Left "Invalid let syntax; first argument must be list of identifier, expression pairs"
parseLet es' = Left $ "Invalid let syntax; expected two arguments, got " ++ show (length es')

parseIf :: [Expression'] -> Either String Expression
parseIf [c,t,f] = IfExp <$> parseHelper c <*> parseHelper t <*> parseHelper f
parseIf es' = Left $ "Invalid if syntax; expected three arguments, got " ++ show (length es')

parseCond :: [Expression'] -> Either String Expression
parseCond [ListExp' l] = case mapM extractCondPair l of
    Just cs -> let (bs, vs) = unzip cs in
        CondExp <$> (zip <$> mapM parseHelper bs <*> mapM parseHelper vs)
    Nothing -> Left "Invalid cond syntax; first argument must be list of expression, expression pairs"
parseCond [_] = Left "Invalid cond syntax; first argument must be list of expression, expression pairs"
parseCond es' = Left $ "Invalid cond syntax; expected one argument, got " ++ show (length es')

extractID :: Expression' -> Maybe String
extractID (IDExp' i) = Just i
extractID _ = Nothing

extractLetPair :: Expression' -> Maybe (String, Expression')
extractLetPair (ListExp' [IDExp' i,e']) = Just (i, e')
extractLetPair _ = Nothing

extractCondPair :: Expression' -> Maybe (Expression', Expression')
extractCondPair (ListExp' [e1',e2']) = Just (e1', e2')
extractCondPair _ = Nothing

parse' :: TokenStream -> (Either String Expression', TokenStream)
parse' [] = (Left "Unexpected end of token stream", [])
parse' (Left err:ts) = (Left err, ts)
parse' (Right (IDTok "lambda"):ts) = (Right Lambda, ts)
parse' (Right (IDTok "let"):ts) = (Right Let, ts)
parse' (Right (IDTok "if"):ts) = (Right If, ts)
parse' (Right (IDTok "cond"):ts) = (Right Cond, ts)
parse' (Right (IDTok i):ts) = (Right (IDExp' i), ts)
parse' (Right (StrTok s):ts) = (Right (StrExp' s), ts)
parse' (Right (IntTok n):ts) = (Right (IntExp' n), ts)
parse' (Right (OPTok p):ts) = case matchParen p of
    Nothing -> (Left $ "Failed to deduce matching close-paren for '" ++ [p] ++ "'", ts)
    Just p' -> parseList p' ts
parse' (Right t:ts) = (Left $ "Unexpected token: '" ++ show t ++ "'", ts)

parseList :: Token -> TokenStream -> (Either String Expression', TokenStream)
parseList p ts = parseList' p ts []

parseList' :: Token -> TokenStream -> [Expression'] -> (Either String Expression', TokenStream)
parseList' p [] _ = (Left $ "Unexpected end of token stream; was expecting '" ++ show p ++ "'", [])
parseList' _ (Left err:ts) _ = (Left err, ts)
parseList' p (Right (CPTok p'):ts) es'
    | CPTok p' == p = (Right $ ListExp' $ reverse es', ts)
    | otherwise = (Left $ "Mismatched close-paren: '" ++ show p' ++ "'; was expecting '" ++ show p ++ "'", [])
parseList' p ts es' = case parse' ts of
    (Left err, ts') -> (Left err, ts')
    (Right e', ts') -> parseList' p ts' (e':es')
