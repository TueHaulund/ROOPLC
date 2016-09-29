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

import Control.Monad.Except

type Error = String

main :: IO ()
main =
    do input <- Str.readFile "example.rpl"
       case compileProgram $ C.unpack input of
           Left err -> putStr $ err ++ "\n"
           Right p -> putStr $ showProgram p
           --Right p -> writeProgram "example.pal" p

compileProgram :: String -> Either Error Program
compileProgram s =
    runExcept $
    parseString s
    >>= classAnalysis
    >>= scopeAnalysis
    >>= typeCheck
    >>= generatePISA
    >>= expandMacros