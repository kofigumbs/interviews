import Data.List (intercalate)
import Data.Set (Set)
import System.Environment (getArgs)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.Parsec.Token (makeTokenParser, naturalOrFloat)

import qualified Data.Set as Set
import qualified Text.Parsec as P


main :: IO ()
main =
  -- Allow multiple files to be passed and parse them in order.
  -- In practice, I think only a single file will be passed.
  mapM_ parseFile =<< getArgs


parseFile :: String -> IO ()
parseFile fileName =
  do  result <- parseFromFile solidStats fileName
      case result of
        Left err ->
          -- TODO print something nicer
          print err

        Right stats ->
          do  putStrLn $ "Number of Triangles: " ++ show (Set.size (triangles stats))
              putStrLn $ "Surface Area: " ++ show (surfaceArea stats)
              putStrLn $ "Bounding Box: " ++ join ", " (calculateBounds stats)


join :: Show a => String -> [a] -> String
join separator list =
  intercalate separator $ show <$> list



-- Math stuff
--


calculateBounds :: Stats -> [Vec]
calculateBounds stats =
    permuteVecs (boundingBoxMin stats) (boundingBoxMax stats)


calculateTriangleArea :: Vec -> Vec -> Vec -> Double
calculateTriangleArea v1 v2 v3 =
  -- <https://en.wikipedia.org/wiki/Heron%27s_formula>
  let
    a = calculateDistance v1 v2
    b = calculateDistance v2 v3
    c = calculateDistance v3 v1
    s = (a + b + c) / 2
  in
  sqrt $ s * (s - a) * (s - b) * (s - c)


calculateDistance :: Vec -> Vec -> Double
calculateDistance (Vec x1 y1 z1) (Vec x2 y2 z2) =
  -- <https://en.wikipedia.org/wiki/Distance#Mathematics>
  sqrt $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)



-- Convenience for working with 3-number sets, like vectors and coordinates
--
data Vec =
  Vec Double Double Double
  deriving (Eq, Ord)


instance Show Vec where
  show (Vec x y z) =
    "{ x: " ++ show x ++ ", y: " ++ show y ++ ", z: " ++ show z ++ " }"


permuteVecs :: Vec -> Vec -> [Vec]
permuteVecs (Vec x1 y1 z1) (Vec x2 y2 z2) =
  -- Use the List Monad to calculate x,y,z permutations
  Vec <$> [ x1, x2 ] <*> [ y1, y2 ] <*> [ z1, z2 ]


combineVecsWith :: (Double -> Double -> Double) -> Vec -> Vec -> Vec
combineVecsWith f (Vec x1 y1 z1) (Vec x2 y2 z2) =
  Vec (f x1 x2) (f y1 y2) (f z1 z2)



-- Parse, record, and combine metrics incrementally
--
-- The main nuance with the usage of this type is its implementation of
-- Semigroup. Each time we parse a facet, we build a Stats record and combine
-- it with whatever stats we've already seen. This makes it convenient to have
-- a "zero Stats" (hence Monoid) because now we can think about Stats as
-- building blocks.
--
data Stats =
  Stats
    { triangles :: Set Triangle
    , surfaceArea :: Double
    , boundingBoxMin :: Vec
    , boundingBoxMax :: Vec
    }


instance Monoid Stats where
  mempty = Stats Set.empty 0 (Vec 0 0 0) (Vec 0 0 0)


instance Semigroup Stats where
  a <> b =
    Stats
      (triangles a <> triangles b)
      (surfaceArea a + surfaceArea b)
      (combineVecsWith min (boundingBoxMin a) (boundingBoxMin b))
      (combineVecsWith max (boundingBoxMax a) (boundingBoxMax b))


vecToStats :: Vec -> Stats
vecToStats vec =
  mempty { boundingBoxMin = vec, boundingBoxMax = vec }


data Triangle =
  Triangle (Set Vec)
  deriving (Eq, Ord)


newTriangle :: Vec -> Vec -> Vec -> Triangle
newTriangle v1 v2 v3 =
  Triangle $ Set.fromList [ v1, v2, v3 ]



-- Parsing stuff
--


chainStats :: Stats -> Parser Stats -> Parser Stats
chainStats previousStats newStats =
  P.choice
    [ do  stats <- newStats
          let candidateStats = stats <> previousStats
          if triangles candidateStats == triangles previousStats
             then P.parserFail "EXPLODE"
             else chainStats candidateStats newStats
    , return previousStats
    ]
      
      
solidStats :: Parser Stats
solidStats =
  do  name <- keyword "solid" *> P.many1 P.letter
      stats <- chainStats mempty facetStats
      keyword "endsolid" <* keyword name <* P.eof
      return stats


facetStats :: Parser Stats
facetStats =
  do  _normal <- keyword "facet" *> keyword "normal" *> vec
      v1 <- keyword "outer loop" *> vertex
      v2 <- vertex
      v3 <- vertex <* keyword "endloop" <* keyword "endfacet"
      return $ (vecToStats v1 <> vecToStats v2 <> vecToStats v3)
        { triangles = Set.singleton (newTriangle v1 v2 v3)
        , surfaceArea = calculateTriangleArea v1 v2 v3
        }


vertex :: Parser Vec
vertex =
  keyword "vertex" *> vec


vec :: Parser Vec
vec =
  Vec <$> number <*> number <*> number


number :: Parser Double
number =
  withSpaces $
    do  sign <- P.option id (negate <$ P.string "-")
        sign <$> either fromIntegral id <$> naturalOrFloat token
        -- 👆 Grab the optional minus sign and use it as a multiplier for the
        -- following number or float that comes next. Either way, cast the result
        -- to a float (Parsec calls them "floats" but uses the Double type).
  where
    token = makeTokenParser emptyDef


keyword :: String -> Parser String
keyword name =
  withSpaces $ P.string name


withSpaces :: Parser a -> Parser a
withSpaces a =
  P.spaces *> a <* P.spaces
