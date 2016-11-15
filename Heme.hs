module Main where

import Tokenizer (Token, tokenize)
import Parser (parse)
import Interpreter (interpret)
import Environment (defaultEnv)

import System.IO (hFlush, stdout)

prompt :: IO ()
prompt = do
    putStr "Heme > "
    hFlush stdout

repl :: [Either String Token] -> IO ()
repl ts = do
    prompt
    let (e, ts') = parse ts in
        case e >>= interpret defaultEnv of
            Left "EOF HACK" -> return ()
            Left err -> do
                putStrLn $ "Error: " ++ err
                repl ts'
            Right foo -> do
                print foo
                repl ts'

main :: IO ()
main = getContents >>= repl . tokenize
