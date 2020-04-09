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
    { _facet_normal :: Vec
    , _facet_loop_1 :: Vec
    , _facet_loop_2 :: Vec
    , _facet_loop_3 :: Vec
    }
  deriving (Show)


data Vec =
  Vec
    { _vec_x :: Double
    , _vec_y :: Double
    , _vec_z :: Double
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
    <$> (keyword "facet" *> keyword "normal" *> vec)
    <*> (keyword "outer loop" *> keyword "vertex" *> vec)
    <*> (keyword "vertex" *> vec)
    <*> (keyword "vertex" *> vec <* keyword "endloop" <* keyword "endfacet")


vec :: Parser Vec
vec =
  Vec <$> number <*> number <*> number


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
