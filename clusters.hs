import System.Environment
import Debug.Trace

import qualified Util as Util
import qualified Graph as Graph
import qualified XML as XML

---- Data representation ----
data Class = Class { vendor :: String,
                     name :: String,
                     family :: String,
                     deviceType :: String }
             deriving (Show)

data Expression = Literal String | 
                  Range Int Int | 
                  GreaterThan Int | 
                  LessThan Int
                  deriving (Show)

data TestMap = TestMap [(String, [Expression])]
               deriving (Show)

data ResponseMap = ResponseMap [(String, TestMap)]
                   deriving (Show)

data Fingerprint = Fingerprint { id :: String,
                                 classes :: [Class],
                                 responses :: ResponseMap }
                   deriving (Show)

---- Operations: ----
compare :: Expression -> Expression -> Bool
compare (Literal x) (Literal y) = x == y
compare (Literal x) (Range a b) = a <= n && n <= b
    where n = Util.hexToInt x
compare (Literal x) (LessThan y) = n < y
    where n = Util.hexToInt x
compare (Literal x) (GreaterThan y) = n > y
    where n = Util.hexToInt x
compare (GreaterThan x) (Literal y) = Main.compare (Literal y) 
                                                   (GreaterThan x)
compare (GreaterThan x) (Range a b) = x >= a
compare (GreaterThan x) (GreaterThan y) = True
compare (GreaterThan x) (LessThan y) = x < y
compare (LessThan x) (Literal y) = Main.compare (Literal y) 
                                                (LessThan x)
compare (LessThan x) (Range a b) = x <= b
compare (LessThan x) (GreaterThan y) = Main.compare (GreaterThan y)
                                                    (LessThan x)
compare (LessThan x) (LessThan y) = True
compare (Range a b) (Literal y) = Main.compare (Literal y) (Range a b)
compare (Range a b) (Range c d) = c <= b && a <= d
compare (Range a b) (GreaterThan y) = Main.compare (GreaterThan y)
                                                   (Range a b)
compare (Range a b) (LessThan y) = Main.compare (LessThan y)
                                                (Range a b)

similarity :: Fingerprint -> Fingerprint -> Float
similarity (Fingerprint _ _ (ResponseMap a)) 
           (Fingerprint _ _ (ResponseMap b)) = 
        compareResponseMaps a b 
    where compareResponseMaps ((k,(TestMap v)):xs) b = 
            (case lookup k b of
                Nothing -> 0.0
                Just (TestMap map) -> (compareTestMaps v map)) +
            (compareResponseMaps xs b)
          compareResponseMaps [] _ = 0.0
          compareTestMaps ((k,v):xs) b = 
            (case lookup k b of
                Nothing -> 0.0
                Just a -> (compareExpressionLists v a)) +
            (compareTestMaps xs b)
          compareTestMaps [] _ = 0.0
          compareExpressionLists xs ys 
            | or $ map (\x -> Main.compare (fst x) (snd x)) 
                       (Util.join xs ys) = 1.0
            | otherwise = 0.0

distance :: Fingerprint -> Fingerprint -> Float
distance a b = (similarity a a) - (similarity a b)

---- Parsing ----

-- token, input, state, current, output, return
sections :: String -> [String] -> Int -> [String] -> [[String]] -> [[String]]
sections token (x:xs) state current output 
    -- entering section:
    | (state == 0) && 
      (not (null x)) &&
      (Util.beginsWith token x) = sections token xs 1 (current ++ [x]) output
    -- inside section:
    | (state == 1) &&
      (not (null x)) = sections token xs state (current ++ [x]) output 
    -- leaving section:
    | (state == 1) && 
      (null x) = sections token xs 0 [] (output ++ [current])
    -- any other case:
    | otherwise = sections token xs state current output
sections token [] state current output = (output ++ [current])

parse :: [String] -> String -> [Class] -> [(String, TestMap)] -> Fingerprint
parse (x:xs) id classes responses
    -- XXX ignore CPE lines
    | (Util.beginsWith "CPE " x) = parse xs id classes responses
    | (Util.beginsWith fp x) = parse xs (drop (length fp) x) classes responses
    | (Util.beginsWith cls x) = parse xs id (classes ++ [parseClass x]) responses
    | otherwise = parse xs id classes (responses ++ [parseResponse x])
    where fp = "Fingerprint "
          cls = "Class "
          parseClass x 
            | (Util.beginsWith cls x) = parseClass (drop (length cls) x)
            | otherwise = let tokens = map Util.trim $ Util.split x '|' 
                          in Class (tokens !! 0)
                                   (tokens !! 1)
                                   (tokens !! 2)
                                   (tokens !! 3)
          parseResponse x 
            | otherwise = (takeWhile (/= '(') x,
                             TestMap (parseTests $ takeWhile (/= ')') 
                                                 $ tail 
                                                 $ dropWhile (/= '(') x))
          parseTests x = map parseTest $ Util.split x '%'
          parseTest x 
            | otherwise = let pair = Util.split x '=' 
                        in (pair !! 0, parseExpressions (pair !! 0))
          parseExpressions x = map parseExpression $ Util.split x '|'
          parseExpression x
            | null x = Literal ""
            | '-' `elem` x = let r = Util.split x '-'
                             in Range (Util.hexToInt (r !! 0))
                                      (Util.hexToInt (r !! 1))
            | '>' == (head x) = GreaterThan $ Util.hexToInt $ tail x
            | '<' == (head x) = LessThan $ Util.hexToInt $ tail x
            | otherwise = Literal x
parse [] id classes responses = Fingerprint id classes (ResponseMap responses)

fingerprints :: [String] -> [Fingerprint]
fingerprints a = map (\s -> parse s "" [] []) 
                     (sections "Fingerprint" a 0 [] [])

---- Output generation ----

number :: (Int, a) -> String
number (n,_) = "n" ++ (show n)

node :: (Int, Fingerprint) -> Graph.Statement
node x@(n,f) = Graph.Node (number x) [Graph.Label $ Main.id f]

nodes :: [(Int, Fingerprint)] -> [Graph.Statement]
nodes = map node

edge :: (Int, a) -> (Int, a) -> Graph.Statement
edge a b = Graph.Edge Graph.NonDirected (number a) (number b) []

edges :: [[(Int, a)]] -> [Graph.Statement]
edges xs = 
    foldr (\x a -> map (\y -> edge (fst y) (snd y)) (Util.connect x) ++ a) [] xs

color :: Int -> Int -> Int -> Graph.Attribute
color red green blue = Graph.Color ("#" ++ (Util.fixed (Util.toHexa red) 2) ++
                                           (Util.fixed (Util.toHexa green) 2) ++
                                           (Util.fixed (Util.toHexa blue) 2))

clusters :: [[(Int, Fingerprint)]] -> Int -> Int -> [Graph.Statement]
clusters (x:xs) n max
    | (length x) > 1 = Graph.SubGraph 
                            ("cluster_" ++ (show n)) 
                            (Graph.Attributes 
                                "graph" 
                                [Graph.Style "filled",
                                 color (div ((max - (length x)) * 255) max)
                                       (div ((max - (length x)) * 255) max) 
                                       (div ((max - (length x)) * 255) max)] 
                             : (nodes x)) 
                       : (clusters xs (n + 1) max)
    | otherwise = (nodes x) ++ (clusters xs n max)
clusters [] _ _ = []

graph :: [[(Int,Fingerprint)]] -> Graph.Graph
graph xs = Graph.Graph Graph.NonDirected "clusters" $
              [Graph.Attributes "node" [Graph.Color "#666666",
                                        Graph.Style "filled",
                                        Graph.Shape "box",
                                        Graph.Fontname "Trebuchet MS",
                                        Graph.Fillcolor "white",
                                        Graph.Fontcolor "#666666"],
               Graph.Attributes "edge" [Graph.Color "#666666",
                                        Graph.Fontname "Trebuchet MS",
                                        Graph.Fontsize 11]] ++
              --(nodes $ concat xs) ++ (edges xs)
              (clusters xs 0 $ maximum [length x | x <- xs])

elements :: [[(Int, Fingerprint)]] -> [XML.Element]
elements (x:xs) 
    | (length x) > 1 = XML.Element "cluster" [] (map element x) : (elements xs)
    | otherwise = element (head x) : (elements xs)
    where element (n,f) = XML.Element "os" [XML.Attribute "name" (Main.id f)] []
elements [] = []

document :: [[(Int,Fingerprint)]] -> XML.Document
document xs = XML.Document $ XML.Element "clusters" [] (elements xs)

-- [1,2,3] 3 -> [1,2,3,1,2,3,1,2,3]
multiply :: [a] -> Int -> [a]
multiply xs n = concat $ take n $ repeat xs

groupBy _ [] = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
  where (ys,zs) = span (eq x) xs

buildGraph input threshold = (length groups, graph groups)
    where groups = groupBy 
                        (\a b -> (distance (snd a) (snd b)) < threshold) 
                        list
          list = zip [0..] $ fingerprints $ lines input

main = do
        (filename:threshold:output:_) <- getArgs
        database <- readFile filename
        let (count, graph) = buildGraph database (read threshold :: Float)
        writeFile output (show graph)
        putStrLn $ "Clusters count: " ++ (show count)
