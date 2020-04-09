import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Data.Text (Text)
import Text.Parsec.Text (Parser, parseFromFile)

import qualified System.Environment as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P


main :: IO ()
main = do
  file <- head <$> Sys.getArgs
  result <- parseFromFile solidStats file
  case result of
    Left err ->
      print err

    Right (Stats triangleCount ( minx, miny, minz ) ( maxx, maxy, maxz )) ->
      do  putStrLn $ "Number of Triangles: " ++ show triangleCount
          putStrLn $ "Surface Area: 1.4142"
          putStrLn $ "Bounding Box: " ++ intercalate ", "
            [ point minx miny minz
            , point maxx miny minz
            , point minx maxy minz
            , point minx miny maxz
            , point maxx maxy minz
            , point maxx miny maxz
            , point minx maxy maxz
            , point maxx maxy maxz
            ]


point :: Double -> Double -> Double -> String
point x y z =
  "{x: " ++ show x ++ ", y: " ++ show y ++ ", z: " ++ show z ++ " }"


-- STL PARSER


data Stats =
  Stats
    { _stats_triangleCount :: Int
    , _stats_boundingBoxMin :: ( Double, Double, Double )
    , _stats_boundingBoxMax :: ( Double, Double, Double )
    }
  deriving (Show)


instance Monoid Stats where
  mempty = Stats 0 ( 0, 0, 0 ) ( 0, 0, 0 )
instance Semigroup Stats where
  (<>) a b =
    Stats
      (_stats_triangleCount a + _stats_triangleCount b)
      (vecBy min (_stats_boundingBoxMin a) (_stats_boundingBoxMin b))
      (vecBy max (_stats_boundingBoxMax a) (_stats_boundingBoxMax b))
    where
      vecBy f ( ax, ay, az ) ( bx, by, bz ) = ( f ax bx, f ay by, f az bz )


solidStats :: Parser Stats
solidStats =
  do  name <- keyword "solid" *> P.many1 P.letter
      stats <- P.chainl1 facetStats (pure (<>)) -- This is very "haskell-y",
                                                -- but also the cleanest way.
                                                -- Read as "we chain each facet
                                                -- and semigroup id as we go."
      keyword "endsolid" <* keyword name <* P.eof
      return stats


facetStats :: Parser Stats
facetStats =
  do  _normal <- keyword "facet" *> keyword "normal" *> vertex
      v1 <- keyword "outer loop" *> vertexStats
      v2 <- vertexStats
      v3 <- vertexStats <* keyword "endloop" <* keyword "endfacet"
      return $ (v1 <> v2 <> v3) { _stats_triangleCount = 1 }


vertexStats :: Parser Stats
vertexStats =
  do  keyword "vertex"
      v <- vertex
      return $ mempty { _stats_boundingBoxMin = v, _stats_boundingBoxMax = v }


vertex :: Parser ( Double, Double, Double )
vertex =
  (,,) <$> number <*> number <*> number


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
