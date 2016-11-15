module Parser where

import Tokenizer (Token(..), matchParen)

data Expression =
    VarExp String | StrExp String | IntExp Integer | AppExp [Expression]
    deriving (Eq, Show)

parse :: [Either String Token] -> (Either String Expression, [Either String Token])
parse [] = (Left "Unexpected end of token stream", [])
parse (Left err:ts) = (Left err, ts)
parse (Right t:ts) = parse' t ts

parse' :: Token -> [Either String Token] -> (Either String Expression, [Either String Token])
parse' (IDTok i) ts = (Right (VarExp i), ts)
parse' (StrTok s) ts = (Right (StrExp s), ts)
parse' (IntTok n) ts = (Right (IntExp n), ts)
parse' (OPTok p) ts = let p' = matchParen p in case p' of
    Nothing -> (Left ("Failed to deduce matching close-paren for '" ++ [p] ++ "'"), ts)
    Just c -> parseList c ts
parse' (CPTok p) ts = (Left ("Unexpected close-paren: '" ++ [p] ++ "'"), ts)

parseList :: Token -> [Either String Token] -> (Either String Expression, [Either String Token])
parseList p ts = parseList' p ts []

parseList' :: Token -> [Either String Token] -> [Expression] -> (Either String Expression, [Either String Token])
parseList' p [] _ = (Left ("Unexpected end of token stream; was expecting '" ++ show p ++ "'"), [])
parseList' _ (Left err:ts) _ = (Left err, ts)
parseList' p (Right t:ts) es = if t == p
    then (Right $ AppExp $ reverse es, ts)
    else case parse (Right t:ts) of
        (Left err, ts') -> (Left err, ts')
        (Right e, ts') -> parseList' p ts' (e:es)
