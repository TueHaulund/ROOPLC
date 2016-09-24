module Main where

import AST
import Parser
import PISA
import ClassAnalyzer
import ScopeAnalyzer
import TypeChecker

import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as C

main :: IO ()
main =
    do input <- Str.readFile "example.rpl"
       case parseString $ C.unpack input of
           Left err -> print err
           Right p -> case classAnalysis p of
                           Left err -> putStr err
                           Right p' -> case scopeAnalysis p' of
                                           Left err -> putStr err
                                           Right p'' -> case typeCheck p'' of
                                                            Left err -> putStr err
                                                            Right _ -> putStr $ printAST $ fst p''