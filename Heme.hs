module Main where

import Tokenizer (TokenStream, tokenize)
import Parser (parse)
import Interpreter (interpret)
import Environment (defaultEnv, Environment, Value(Void))

import System.IO (hFlush, stdout)
import Control.Monad (unless)

prompt :: IO ()
prompt = do
    putStr "Heme > "
    hFlush stdout

repl :: Environment -> TokenStream -> IO ()
repl env ts = do
    prompt
    let (e, ts') = parse ts in
        case e >>= interpret env of
            Left "EOF HACK" -> return ()
            Left err -> do
                putStrLn $ "Error: " ++ err
                repl env ts'
            Right (env',val) -> do
                unless (val == Void) $ print val
                repl env' ts'

eval :: Environment -> TokenStream -> Either String Environment
eval env ts = let (e, ts') = parse ts in
    case e >>= interpret env of
        Left "EOF HACK" -> Right env
        Left err -> Left err
        Right (env',_) -> eval env' ts'

main :: IO ()
main = do
    defaults <- readFile "defaults.heme"
    case eval defaultEnv $ tokenize defaults of
        Right env -> getContents >>= repl env . tokenize
        Left err -> putStr $ "defaults.heme: " ++ err
    putStr "\n"
