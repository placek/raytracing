module STL where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Geometry
import Vector

stlCoerce (Facet _ ((Vertex a b c), (Vertex d e f), (Vertex g h i))) = Triangle (Vector (_n a) (_n b) (_n c)) (Vector (_n d) (_n e) (_n f)) (Vector (_n g) (_n h) (_n i))

data Name   = Name String deriving (Show)
data Number = Number { _n :: Double } deriving (Show)
data Normal = Normal Number Number Number deriving (Show)
data Vertex = Vertex Number Number Number deriving (Show)
data Facet  = Facet Normal (Vertex, Vertex, Vertex) deriving (Show)
data STL    = STL Name [Facet] deriving (Show)

(<++>) a b = (++) <$> a <*> b
(<:>) a b  = (:) <$> a <*> b

whitespace = space <|> tab

newLine = do
            endOfLine
            many whitespace

float = integer <++> decimal <++> exponent
  where number   = many1 digit
        plus     = char '+' *> number
        minus    = char '-' <:> number
        integer  = plus <|> minus <|> number
        decimal  = option "" $ char '.' <:> number
        exponent = option "" $ oneOf "eE" <:> integer

parseName :: Parser Name
parseName = do
              x <- many1 (letter <|> char '_')
              return $ Name x

parseNumber :: Parser Number
parseNumber = do
                x <- float
                return $ Number (read x)

parseNormal :: Parser Normal
parseNormal = do
                string "normal"
                many1 whitespace
                x <- parseNumber
                many1 whitespace
                y <- parseNumber
                many1 whitespace
                z <- parseNumber
                return $ Normal x y z

parseVertex :: Parser Vertex
parseVertex = do
                string "vertex"
                many1 whitespace
                x <- parseNumber
                many1 whitespace
                y <- parseNumber
                many1 whitespace
                z <- parseNumber
                return $ Vertex x y z

parseFacet :: Parser Facet
parseFacet = do
               string "facet"
               many1 whitespace
               n <- parseNormal
               newLine
               string "outer loop"
               newLine
               x <- parseVertex
               newLine
               y <- parseVertex
               newLine
               z <- parseVertex
               newLine
               string "endloop"
               newLine
               string "endfacet"
               newLine
               return $ Facet n (x, y,z)

parseSTL :: Parser STL
parseSTL = do
             string "solid"
             many1 whitespace
             n <- parseName
             newLine
             fs <- manyTill parseFacet (string "endsolid")
             many endOfLine
             eof
             return $ STL n fs
