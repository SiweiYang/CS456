module Main where

import Router (RouteEdge(RouteEdge), RoutePath(RoutePath), edgeToPath)
import RouterIO (readEdges, readPaths, cleanPaths, writePaths, proposeEdge, excludeNode)

import System.Environment (getArgs)
import System.IO.Error (catch, IOError)
import Data.Maybe (Maybe(..), maybeToList, catMaybes)


main :: IO ()
main = do
         v:_ <- getArgs
         edges <- readEdges "edges" v
         --putStrLn (show edges)
         let dests = map (\(RouteEdge v1 v2 ecost) -> v2) edges
         paths <- mapM readPaths dests
         --putStrLn (show paths)
         let paths' = concat ((map edgeToPath edges):paths)
         --putStrLn (show (map (\edge -> proposeEdge paths' edge) edges))
         --let paths'' = foldr (\edge paths -> proposeEdge paths edge) paths' edges
         let paths'' = concat (map (\edge -> proposeEdge paths' edge) edges)
         --putStrLn (show paths'')
         let paths''' = cleanPaths (excludeNode paths'' v)
         --putStrLn (show paths''')
         writePaths v paths'''
         
         return ()