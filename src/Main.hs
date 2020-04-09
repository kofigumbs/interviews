import Data.List (intercalate)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Language (emptyDef)

import qualified System.Environment as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T


main :: IO ()
main =
  -- Allow multiple files to be passed and parse them in order.
  -- In practice, I think only a single file will be passed.
  mapM_ parseFile =<< Sys.getArgs


parseFile :: String -> IO ()
parseFile fileName =
  do  result <- parseFromFile solidStats fileName
      case result of
        Left err ->
          print err

        Right stats ->
          do  putStrLn $ "Number of Triangles: " ++ show (triangleCount stats)
              putStrLn $ "Surface Area: " ++ calculateSurfaceArea stats
              putStrLn $ "Bounding Box: " ++ calculateBoundingBox stats


calculateSurfaceArea :: Stats -> String
calculateSurfaceArea _stats =
  show 1.4142


calculateBoundingBox :: Stats -> String
calculateBoundingBox stats =
  intercalate ", " $ show <$>
    permuteVecs (boundingBoxMin stats) (boundingBoxMax stats)


-- Convenience for working with 3-number sets, like vectors and coordinates
--
data Vec =
  Vec Double Double Double


instance Show Vec where
  show (Vec x y z) =
    "{ x: " ++ show x ++ ", y: " ++ show y ++ ", z: " ++ show z ++ " }"


permuteVecs :: Vec -> Vec -> [Vec]
permuteVecs (Vec ax ay az) (Vec bx by bz) =
  -- Use the List Monad to calculate x,y,z permutations
  Vec <$> [ ax, bx ] <*> [ ay, by ] <*> [ az, bz ]



-- Parse, record, and combine metrics incrementally 
--
data Stats =
  Stats
    { triangleCount :: Int
    , boundingBoxMin :: Vec
    , boundingBoxMax :: Vec
    }


instance Monoid Stats where
  mempty = Stats 0 (Vec 0 0 0) (Vec 0 0 0)


instance Semigroup Stats where
  a <> b =
    Stats
      (triangleCount a + triangleCount b)
      (vecBy min (boundingBoxMin a) (boundingBoxMin b))
      (vecBy max (boundingBoxMax a) (boundingBoxMax b))
    where
      vecBy f (Vec ax ay az) (Vec bx by bz) = Vec (f ax bx) (f ay by) (f az bz)


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
      return $ (v1 <> v2 <> v3) { triangleCount = 1 }


vertexStats :: Parser Stats
vertexStats =
  do  keyword "vertex"
      v <- vertex
      return $ mempty { boundingBoxMin = v, boundingBoxMax = v }


vertex :: Parser Vec
vertex =
  Vec <$> number <*> number <*> number


number :: Parser Double
number =
  -- Parsec doesn't include negative number parsers by default
  -- <https://hackage.haskell.org/package/parsec-numbers>
  withSpaces $
    do  sign <- P.option id (negate <$ P.string "-")
        sign <$> either fromIntegral id <$> T.naturalOrFloat token


keyword :: String -> Parser String
keyword name =
  withSpaces $ P.string name


withSpaces :: Parser a -> Parser a
withSpaces a =
  P.spaces *> a <* P.spaces


token :: T.TokenParser a
token =
  T.makeTokenParser emptyDef
