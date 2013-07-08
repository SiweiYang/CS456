module RouterIO where

import Router (RouteEdge(RouteEdge), RoutePath(RoutePath), edgeToPath)

import System.Environment (getArgs)
import System.IO.Error (catch, IOError)
import Data.Maybe (Maybe(..), maybeToList, catMaybes)

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

writePaths :: String -> [RoutePath] -> IO ()
writePaths v paths = do
                      let out = if length paths > 0 then content else ""
                      writeFile ("table." ++ v) out
                    where
                      p:ps = paths
                      content = foldr (\path out -> out ++ "\n" ++ (show path)) (show p) ps

proposeEdge :: [RoutePath] -> RouteEdge -> [RoutePath]
proposeEdge paths (RouteEdge v1 v2 ecost) = filter (\(RoutePath path pcost) -> head path == v1) path'
                                            where
                                              applyEdge (RoutePath path pcost) = if head path == v2 then RoutePath (v1:path) (pcost + ecost) else RoutePath path pcost
                                              path' = map applyEdge paths

excludeNode :: [RoutePath] -> String -> [RoutePath]
excludeNode paths v = filter (\(RoutePath path pcost) -> notElem v path) paths


main :: IO ()
main = do
         v:_ <- getArgs
         edges <- readEdges "edges" v
         let dests = map (\(RouteEdge v1 v2 ecost) -> v2) edges
         paths <- mapM readPaths dests
         let paths' = concat ((map edgeToPath edges):paths)
         writePaths v paths'
         putStrLn (show paths')
         return ()