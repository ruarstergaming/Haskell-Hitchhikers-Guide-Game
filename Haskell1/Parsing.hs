{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 |||

{-
The monad of parsers
--------------------
-}

newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
   fmap f p = do p' <- p
                 return (f p')

instance Applicative Parser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   empty = mzero
   p <|> q = p ||| q

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()
{-
Ignoring spacing
----------------
-}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)



parseDirection :: Parser Direction 
parseDirections 
     = do string "north" ||| string "North"
          return North 
   ||| do string "south" ||| string "South"
          return South 
   ||| do string "east"  ||| string "East"
          return East  
   ||| do string "west"  ||| string "West"
          return West
   ||| do string "out"  ||| string "Out"
          return Out

parseObject :: Parser Object 
parseDirections 
     = do string "mug" 
          return Mug 
   ||| do string "fullmug"
          return FullMug  
   ||| do string "coffeepot"
          return coffeepot
   ||| do string "key"
          return Key 
   ||| do string "mask"
          return Mask 
   ||| do string "wallet"
          return Wallet 
   ||| do String "matric"
          return Matric 
   ||| do String "mazeMap"
          return MazeMap 
   ||| do String "door"
          return Door
          
        

--Command syntax : = "go" <direction>
parseCommand :: Parser Command 
parseCommand
   = do string "go" |||  string "Go"
        space
        d <- parseDirection
        return (Go d)
   ||| do string "get" ||| string "Get"
          space 
          a <- parseObject 
          return (Get a)
   ||| do string "drop" ||| string "Drop"
          space 
          a <- parseObject 
          return (Drop a)
   ||| do string "Examine" ||| string "examine"
          space 
          a <- parseObject 
          return (Examine a)
   ||| do string "open" ||| string "Open"
          space 
          a <- parseObject 
          return (Examine a)

runParser :: String -> Maybe Command
runParser input = case parse parseCommand input of
                  [(cmd, "")] -> Just cmd
                  _ -> Nothing