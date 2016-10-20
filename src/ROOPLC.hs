import Control.Monad.Except

import Parser
import PISA
import ClassAnalyzer
import ScopeAnalyzer
import TypeChecker
import CodeGenerator
import MacroExpander

type Error = String

main :: IO ()
main =
    do input <- getContents
       case compileProgram input of
           Left err -> putStr $ err ++ "\n"
           Right p -> putStr $ showProgram p

compileProgram :: String -> Either Error Program
compileProgram s =
    runExcept $
    parseString s
    >>= classAnalysis
    >>= scopeAnalysis
    >>= typeCheck
    >>= generatePISA
    >>= expandMacros