module Tokenizer where

import GHC.Unicode (isDigit, isSpace)

data Token =
    IDTok String | StrTok String | IntTok Integer | OPTok Char | CPTok Char
    deriving Eq

instance Show Token where
    show (IDTok i) = i
    show (StrTok s) = s
    show (IntTok n) = show n
    show (OPTok p) = [p]
    show (CPTok p) = [p]

type TokenStream = [Either String Token]

tokenize :: String -> TokenStream
tokenize "" = [Left "EOF HACK"]
tokenize (c:cs)
    | isSpace c = tokenize cs
    | c == '"' = let (t, cs') = break (== '"') cs in
        if null cs'
            then [Left $ "Unmatched double quote character in \"" ++ t]
            else (Right $ StrTok t) : tokenize (tail cs')
    | isOpenParen c = (Right $ OPTok c) : tokenize cs
    | isCloseParen c = (Right $ CPTok c) : tokenize cs
    | otherwise = let (t, cs') = break isDelimiter (c:cs) in
        if all isDigit t || (not (null t) && head t == '-' && all isDigit (tail t))
            then (Right $ IntTok $ read t) : tokenize cs'
            else (Right $ IDTok t) : tokenize cs'

isDelimiter :: Char -> Bool
isDelimiter c = isOpenParen c || isCloseParen c || isSpace c || c == '"'

parentheses :: [(Char, Char)]
parentheses = [('(', ')'), ('[', ']'), ('{', '}')]

isOpenParen :: Char -> Bool
isOpenParen = (`elem` map fst parentheses)

isCloseParen :: Char -> Bool
isCloseParen = (`elem` map snd parentheses)

matchParen :: Char -> Maybe Token
matchParen c = CPTok <$> lookup c parentheses
