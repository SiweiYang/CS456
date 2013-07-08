module Router where

data RouteEdge = RouteEdge { from, to :: String, ecost :: Int } deriving (Show)
data RoutePath = RoutePath { path :: [String], pcost :: Int }

instance Show RoutePath where
  show (RoutePath (root:path) pcost) = if length intermediate > 0
                                then dest ++ " " ++ (show pcost) ++ " " ++ out
                                else dest ++ " " ++ (show pcost)
                                where
                                  dest = last path
                                  intermediate = init path
                                  v:vs = intermediate
                                  out = foldr (\v' out -> out ++ " " ++ v') v vs

edgeToPath :: RouteEdge -> RoutePath
edgeToPath (RouteEdge v1 v2 ecost) = (RoutePath (v1:v2:[]) ecost)