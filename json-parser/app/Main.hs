module Main where

import Control.Applicative
import Data.Char
import JsonVal

{- | A data type represents the parsing string into a certain data format
  NOTE: no proper error reporting
-}
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser f1) <*> (Parser p1) =
    Parser $ \input -> do
      (input', f2) <- f1 input
      (input'', p2) <- p1 input'
      return (input'', f2 p2)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
 where
  f "true" = JsonBool True
  f _ = JsonBool False -- Since the range of arguments is restricted to two options, namely "true" or "false", f need not distinct strings other than "true"

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in case token of
        [] -> Nothing
        _ -> Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
 where
  f = JsonNumber . read

stringLiteral :: Parser String
stringLiteral = spanP ('"' /=)

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> spanP ('"' /=) <* charP '"')

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber

charP :: Char -> Parser Char
charP x = Parser f
 where
  f (y : ys)
    | y == x = Just (ys, x)
    | otherwise = Nothing
  f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

main :: IO ()
main = putStrLn "Hello, Haskell!"
