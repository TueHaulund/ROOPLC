import Control.Monad.Except
import System.IO

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
       either (hPutStrLn stderr) (putStr . showProgram) (compileProgram input)

compileProgram :: String -> Either Error Program
compileProgram s =
    runExcept $
    parseString s
    >>= classAnalysis
    >>= scopeAnalysis
    >>= typeCheck
    >>= generatePISA
    >>= expandMacros