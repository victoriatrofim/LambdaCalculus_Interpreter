module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char
import Lambda
import Binding

newtype Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
    return = pure
    mp >>= f = Parser $ \s -> case parse mp s of 
                                [] -> []
                                [(v, rest)] -> parse (f v) rest      

instance Applicative Parser where
    af <*> mp = 
        do 
            f <- af
            v <- mp
            return $ f v
    pure v = Parser $ \s -> [(v, s)]

instance Functor Parser where 
    fmap f mp = 
        do 
            x <- mp
            return $ f x

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                [] -> parse p2 s 
                                x -> x

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) s = p s 

failParser :: Parser a 
failParser = Parser $ \s -> []

charParser :: Char -> Parser Char
charParser c = Parser $ \s -> 
                case s of 
                    [] -> []
                    (x:xs) -> if x == c then [(x,xs)] else [] 

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> 
                       case s of 
                        [] -> []
                        (x:xs) -> if p x then [(x,xs)] else [] 

plusParser :: Parser a -> Parser [a]
plusParser p = do
                x <- p
                xs <- starParser p
                return (x:xs)

starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> pure []

whitespaceParser :: Parser String
whitespaceParser = starParser (charParser ' ')

alpha :: Parser Char
alpha = predicateParser (`elem` ['a'..'z'])

variable :: Parser String
variable = plusParser alpha

lambdaVar :: Parser Lambda
lambdaVar = Var <$> variable

lambdaAbs :: Parser Lambda
lambdaAbs = do
    _ <- charParser '\\'
    var <- variable
    _ <- charParser '.'
    body <- lambdaExpr
    return (Abs var body)

lambdaApp :: Parser Lambda
lambdaApp = do
    _ <- charParser '('
    l1 <- lambdaExpr
    _ <- charParser ' '
    l2 <- lambdaExpr
    _ <- charParser ')'
    return (App l1 l2)

isMacroChar :: Char -> Bool
isMacroChar c = isUpper c || isDigit c

macro :: Parser Lambda
macro = Macro <$> plusParser (predicateParser isMacroChar)

lambdaExpr :: Parser Lambda
lambdaExpr = lambdaVar <|> lambdaAbs <|> lambdaApp <|> macro

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda input = case parse lambdaExpr input of
    [(result, "")] -> result
    _ -> error "Parsing error"

evalLine :: Parser Line
evalLine = Eval <$> lambdaExpr

bindingLine :: Parser Line
bindingLine = do
    _ <- whitespaceParser
    name <- plusParser (predicateParser isMacroChar)
    _ <- whitespaceParser
    _ <- charParser '='
    _ <- whitespaceParser
    expr <- lambdaExpr
    return (Binding name expr)

line :: Parser Line
line = bindingLine <|> evalLine

-- 3.3.
parseLine :: String -> Either String Line
parseLine input = case parse line input of
    [(result, "")] -> Right result
    _ -> Left "Parsing error"
