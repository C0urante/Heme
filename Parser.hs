module Parser where

import Tokenizer (Token(..), matchParen, TokenStream)

data Expression =
    SymExp String | StrExp String | IntExp Integer | ListExp [Expression]
    deriving (Eq, Show)

parse :: TokenStream -> (Either String Expression, TokenStream)
parse [] = (Left "Unexpected end of token stream", [])
parse (Left err:ts) = (Left err, ts)
parse (Right (IDTok i):ts) = (Right (SymExp i), ts)
parse (Right (StrTok s):ts) = (Right (StrExp s), ts)
parse (Right (IntTok n):ts) = (Right (IntExp n), ts)
parse (Right (OPTok p):ts) = case matchParen p of
    Nothing -> (Left $ "Failed to deduce matching close-paren for '" ++ [p] ++ "'", ts)
    Just p' -> parseList p' ts
parse (Right t:ts) = (Left $ "Unexpected token: '" ++ show t ++ "'", ts)

parseList :: Token -> TokenStream -> (Either String Expression, TokenStream)
parseList p ts = parseList' p ts []

parseList' :: Token -> TokenStream -> [Expression] -> (Either String Expression, TokenStream)
parseList' p [] _ = (Left $ "Unexpected end of token stream; was expecting '" ++ show p ++ "'", [])
parseList' _ (Left err:ts) _ = (Left err, ts)
parseList' p (Right (CPTok p'):ts) es'
    | CPTok p' == p = (Right $ ListExp $ reverse es', ts)
    | otherwise = (Left $ "Mismatched close-paren: '" ++ show p' ++ "'; was expecting '" ++ show p ++ "'", [])
parseList' p ts es' = case parse ts of
    (Left err, ts') -> (Left err, ts')
    (Right e', ts') -> parseList' p ts' (e':es')
