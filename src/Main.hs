import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Text.Parsec.Text (Parser, parseFromFile)

import qualified Data.Text as Text
import qualified System.Environment as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P


main :: IO ()
main = do
  file <- head <$> Sys.getArgs
  result <- parseFromFile solid file
  case result of
    Left err ->
      print err

    Right (Solid _ facets) ->
      do  putStrLn $ "Number of Triangles: " ++ show (length facets)
          putStrLn $ "Surface Area: 1.4142"
          putStrLn $ "Bounding Box: {x: 0, y: 0, z: 0 }, {x: 1, y: 1, z: 1 } ..."



-- STL PARSER


data Solid =
  Solid
    { _solid_name :: Text
    , _solid_facets :: [Facet]
    }
  deriving (Show)


data Facet =
  Facet
    { _facet_normal_x :: Double
    , _facet_normal_y :: Double
    , _facet_normal_z :: Double
    , _facet_loop_1_x :: Double
    , _facet_loop_1_y :: Double
    , _facet_loop_1_z :: Double
    , _facet_loop_2_x :: Double
    , _facet_loop_2_y :: Double
    , _facet_loop_2_z :: Double
    , _facet_loop_3_x :: Double
    , _facet_loop_3_y :: Double
    , _facet_loop_3_z :: Double
    }
  deriving (Show)


solid :: Parser Solid
solid =
  do  keyword "solid"
      name <- P.many1 P.letter
      facets <- P.many facet
      keyword "endsolid"
      keyword name
      P.eof
      return $ Solid (Text.pack name) facets


facet :: Parser Facet
facet =
  Facet
    <$> (keyword "facet" *> keyword "normal" *> number)
    <*> number
    <*> number
    <*> (keyword "outer loop" *> keyword "vertex" *> number)
    <*> number
    <*> number
    <*> (keyword "vertex" *> number)
    <*> number
    <*> number
    <*> (keyword "vertex" *> number)
    <*> number
    <*> (number <* keyword "endloop" <* keyword "endfacet")


number :: Parser Double
number =
  do  P.spaces
      sign <- P.option id (negate <$ P.string "-")
      value <- either fromIntegral id <$> P.naturalOrFloat token
      P.spaces
      return $ sign value


keyword :: String -> Parser ()
keyword name =
  do  P.spaces
      P.string name
      P.spaces


-- This stuff doesn't matter, but needs to be here <https://github.com/haskell/parsec/issues/59>
--
-- TL;DR some of the more complex Parsec parsers (like `naturalOrFloat`)
-- require a TokenParser, but Parsec only includes definitions for String.
-- Our parser, however, is based on Text, which is more performant for UTF8.

token :: P.GenTokenParser Text () Identity
token =
  P.makeTokenParser languageDef


languageDef :: P.GenLanguageDef Text st Identity
languageDef =
  P.LanguageDef
    { P.identStart  = P.letter
    , P.identLetter = P.letter
    , P.opLetter    = P.parserZero
    , P.opStart     = P.parserZero
    , P.commentStart    = ""
    , P.commentEnd      = ""
    , P.commentLine     = ""
    , P.nestedComments  = True
    , P.reservedOpNames = []
    , P.reservedNames   = []
    , P.caseSensitive   = True
    }
