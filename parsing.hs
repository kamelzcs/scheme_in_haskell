#! /usr/bin/env runhugs +l
--
-- parsing.hs
-- Copyright (C) 2015 zhao <zhao@kamel-ThinkPad-X201>
--
-- Distributed under terms of the MIT license.
--

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Applicative ( (<$>), )
import Numeric

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Float Double
             | Number Integer
             | String String
             | Bool Bool

escapedChars :: Parser Char
escapedChars = do char '\\' 
                  x <- oneOf "\\\"nrt" 
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
          digits <-  many1 digit
          return $ Number (read  digits)

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst.head$readFloat (x++"."++y))

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseFloat

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
          args <- getArgs
          putStrLn (readExpr (args !! 0))
