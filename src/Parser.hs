module Parser where

import Data.Maybe
import Data.Tuple
import Control.Applicative

import Text.Printf

type OldParser a = String -> Maybe (a, String)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

-- parseSpace / runParser (parseTuple parseInt_) $ removeChar "( 1 2 3 , 4 5 6 )foo bar" ' '

removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar n (x:xs) = if n == x then removeChar n xs
                      else x : removeChar n xs

parseSign :: String -> Char
parseSign ('+':next) = '+'
parseSign ('-':next) = '-'
parseSign ('*':next) = '*'
parseSign ('/':next) = '/'
parseSign ('^':next) = '^'
parseSign _ = undefined

-- parseNoOP :: Parser String
-- parseNoOP = Parser $ parseNoOP_

-- parseNoOP_ :: OldParser String
-- parseNoOP_ [] = Nothing
-- parseNoOP_ str = case parseSome_ (parseAnyChar_ "0123456789.") str of
--     Just (v1,next) -> case parseSome_ (parseAnyChar_ "+-*/^") next of
--         Just (s,next) -> case parseSome_ (parseAnyChar_ "0123456789.") next of
--             Just (v2,end) -> Just (str,"")
--             Nothing -> Nothing
--         Nothing -> case parseSome_ (parseAnyChar_ "0123456789.") str of
--             Just (v2,end) -> Nothing
--             Nothing -> Nothing
--     Nothing -> Nothing

-- runParser (parseChar 'a') "avc"

parseChar :: Char -> Parser Char
parseChar str = Parser $ parseChar_ str

parseChar_ :: Char -> OldParser Char
parseChar_ x [] = Nothing
parseChar_ x (xa:xb)
    | x == xa = Just (x,xb)
    | otherwise = Nothing

-- runParser (parseAnyChar "abc") "avc"

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser $ parseAnyChar_ str

parseAnyChar_ ::  String -> OldParser Char
parseAnyChar_ x [] = Nothing
parseAnyChar_ (x:xn) (y:yn)
    | x == y = Just (x,yn)
    | otherwise = parseAnyChar_ xn (y:yn)
parseAnyChar_ x _ = Nothing

-- runParser (parseOr (parseChar_ 'a') (parseChar_ 'b')) "abcd"

parseOr :: OldParser a -> OldParser a -> Parser a
parseOr x str = Parser $ parseOr_ x str

parseOr_ :: OldParser a -> OldParser a -> OldParser a
parseOr_ a b string
    | isJust (a string) = a string
    | isJust (b string) = b string
    | otherwise = Nothing

-- runParser (parseAnd (parseChar_ 'a') (parseChar_ 'b')) "abcd"

parseAnd :: OldParser a -> OldParser b -> Parser (a, b)
parseAnd a str = Parser $ parseAnd_ a str

parseAnd_ :: OldParser a -> OldParser b -> OldParser (a,b)
parseAnd_ a b string = case a string of
    Nothing -> Nothing
    Just (a,string) -> case b string of
        Just (b,string) -> Just ((a,b), string)
        Nothing -> Nothing

-- runParser (parseAndWith (\ x y -> [x,y]) (parseChar_ 'a') (parseChar_ 'b')) "abcd"

parseAndWith :: (a1 -> b -> a2) -> OldParser a1 -> OldParser b -> Parser a2
parseAndWith a b str = Parser $ parseAndWith_ a b str

parseAndWith_ :: (a -> b -> c) -> OldParser a -> OldParser b -> OldParser c
parseAndWith_ fonction a b string = case a string of
    Nothing -> Nothing
    Just (a,string) -> case b string of
        Just (b,string) -> Just ((fonction a b),string)
        Nothing -> Nothing

-- runParser (parseMany (parseChar_ ' ')) "    foobar"

recursive :: [a] -> OldParser a -> OldParser [a]
recursive tab a string = case a string of
    Nothing -> Just (tab,string)
    Just (b,next) -> recursive fill a next
        where fill = tab ++ [b]

parseMany :: OldParser a -> Parser [a]
parseMany str = Parser $ parseMany_ str

parseMany_ :: OldParser a -> OldParser [a]
parseMany_ a string = recursive [] a string

-- runParser (parseSome (parseAnyChar_ ['0'..'9'])) "1234567890foobar"

parseSome_ :: OldParser a -> OldParser [a]
parseSome_ a string
    | isJust (a string) = recursive [] a string
    | otherwise = Nothing

parseSome :: OldParser a -> Parser [a]
parseSome a = Parser $ parseSome_ a

-- runParser parseUInt "42"

parseUInt :: Parser Int
parseUInt = Parser $ parseUInt_

parseUInt_ :: OldParser Int -- parse an unsigned Int
parseUInt_ str = case parseChar_ '0' str of
    Nothing -> case parseSome_ (parseAnyChar_ ['0'..'9']) str of
        Just (res,next) -> Just (read res,next)
        Nothing -> Nothing
    Just (x,y) -> Nothing

-- runParser parseInt "42"

parseInt :: Parser Int
parseInt = Parser $ parseInt_

parseInt_ :: OldParser Int -- parse a signed Int
parseInt_ [] = Nothing
parseInt_ string = case parseChar_ '-' string of
    Nothing -> case parseUInt_ string of
        Nothing -> Nothing
        Just (string,next) -> Just (string,next)
    Just ('-',res) -> case parseUInt_ res of
        Nothing -> Nothing
        Just (x,xs) -> Just ((-1)*x,xs)

-- parseFloat

parseFloat :: Parser Float
parseFloat = Parser $ parseFloat_

parseFloat_ :: OldParser Float
parseFloat_ [] = Nothing
parseFloat_ string = case parseChar_ '-' string of
    Nothing -> case parseSome_ (parseAnyChar_ "0123456789.") string of
        Just (x,xs) -> Just (read x :: Float,xs)
        Nothing -> Nothing
    Just ('-',z) -> case parseSome_ (parseAnyChar_ "0123456789.") z of
        Just (u,v) -> Just ((-1)*read u :: Float,v)
        Nothing -> Nothing

-- runParser (parseTuple parseInt_) "(123,456)foo bar"

parseTuple :: OldParser a -> Parser (a, a)
parseTuple a = Parser $ parseTuple_ a

parseTuple_ :: OldParser a -> OldParser (a,a) -- parse a tuple
parseTuple_ a string = case parseChar_ '(' string of
    Just (par,next) -> case a next of
        Just (v1,y) -> case parseChar_ ',' y of
            Just (dot,z) -> case a z of
                Just (v2,last) -> case parseChar_ ')' last of
                    Just (par,end) -> Just ((v1,v2),end)
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

-- step 2.2
-- Functor

instance Functor Parser where
    fmap fct parser = Parser f where
        f str = case runParser parser str of
            Just (a, b) -> Just (fct a, b)
            Nothing -> Nothing
        fct <$> parser = fmap fct parser

instance Applicative Parser where
    pure x = Parser $ \xs -> Just (x, xs)
    Parser p1 <*> pp2 = Parser $ \xs -> case p1 xs of
        Just (f, xs') -> case runParser pp2 xs' of
            Just (a, xs'') -> Just (f a, xs'')
            Nothing -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> p2 = Parser $ \s -> p1 s <|> runParser p2 s

-- Monad Parser