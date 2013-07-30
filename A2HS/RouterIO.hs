module RouterIO where

import Router (RouteEdge(RouteEdge), RoutePath(RoutePath, path, pcost), edgeToPath)

import System.Environment (getArgs)
import System.IO.Error (catch, IOError)
import Data.Maybe (Maybe(..), maybeToList, catMaybes)

-- read available edges and their cost from the specified edge file
readEdges :: String -> String -> IO [RouteEdge]
readEdges filename root = do
                            content <- readFile filename
                            let records = lines content
                            let aggregator edges line = edges ++ (constructor (words line))
                            let edges = foldl aggregator [] records
                            --putStrLn (show edges)
                            return (filter (\(RouteEdge v1 v2 ecost) -> v1 == root) edges)
                          where
                            -- edge file have either empty line or edge line
                            -- edge line has no trailing or following space
                            constructor :: [String] -> [RouteEdge]
                            constructor (v1:v2:cost:[])= [(RouteEdge v1 v2 (read cost)), (RouteEdge v2 v1 (read cost))]
                            constructor []= []

-- 
proposeEdge :: [RoutePath] -> RouteEdge -> [RoutePath]
proposeEdge paths (RouteEdge v1 v2 ecost) = filter (\(RoutePath path pcost) -> head path == v1) path'
                                            where
                                              applyEdge (RoutePath path pcost) = if head path == v2 then RoutePath (v1:path) (pcost + ecost) else RoutePath path pcost
                                              path' = map applyEdge paths

-- read paths from the specified table file
-- silently return empty list when file not exists
readPaths :: String -> IO [RoutePath]
readPaths v = catch readPath' (\e -> print e >> return [])
              where
                readPath' = do
                  content <- readFile ("table." ++ v)
                  let records = lines content
                  let aggregator paths line = (maybeToList (constructor (words line))) ++ paths
                  let paths = foldl aggregator [] records
                  return paths
                -- edge file have either empty line or edge line
                -- edge line has no trailing or following space
                constructor :: [String] -> Maybe RoutePath
                constructor (vd:cost:vs) = Just (RoutePath ((v:vs) ++ [vd]) (read cost))
                constructor []= Nothing

-- deduplication of paths so that for any destination there will be at most one path
-- shortest path out-run others for the same destination
-- if there is a tie, then the name of the starting node on the path is used to break the tie consistently
cleanPaths :: [RoutePath] -> [RoutePath]
cleanPaths paths = foldr (\p paths -> updatePath paths p) [] paths
                   where
                     updatePath :: [RoutePath] -> RoutePath -> [RoutePath]
                     updatePath [] p = [p]
                     updatePath (p:paths) p' = if (head (path p)) == (head (path p')) && (last (path p)) == (last (path p'))
                                               then case of compare (pcost p) > (pcost p')
                                                    GT -> p':paths
                                                    LT -> p:paths
                                                    EQ -> if head (path p)) > (head (path p') then p':paths else p:paths
                                               else p:(updatePath paths p')
                     

-- write paths to the specified table file
-- silently return empty list when file not exists
writePaths :: String -> [RoutePath] -> IO ()
writePaths v paths = do
                      let out = if length paths > 0 then content else ""
                      writeFile ("table." ++ v) out
                    where
                      p:ps = paths
                      content = foldr (\path out -> out ++ "\n" ++ (show path)) (show p) ps

-- filter out all paths that came across the specified node
excludeNode :: [RoutePath] -> String -> [RoutePath]
excludeNode paths v = filter (\(RoutePath path pcost) -> notElem v (tail path)) paths
