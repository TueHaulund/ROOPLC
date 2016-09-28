module Main where

import Parser
import PISA
import ClassAnalyzer
import ScopeAnalyzer
import TypeChecker
import CodeGenerator
import MacroExpander

import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as C

main :: IO ()
main =
    do input <- Str.readFile "example.rpl"
       case parseString $ C.unpack input of
           Left err -> print err
           Right p1 -> case classAnalysis p1 of
                           Left err -> putStr err
                           Right p2 -> case scopeAnalysis p2 of
                                           Left err -> putStr err
                                           Right p3 -> case typeCheck p3 of
                                                           Left err -> putStr err
                                                           Right p4 -> case generatePISA p4 of
                                                                           Left err -> putStr err
                                                                           Right p5 -> case expandMacros p5 of
                                                                                           Left err -> putStr err
                                                                                           Right p6 -> putStr $ showProgram p6