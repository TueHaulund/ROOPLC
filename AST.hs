module AST where

import Data.Map
import Text.Show.Pretty

{-- AST Primitives --}

type TypeName = String
type MethodName = String

data DataType = IntegerType | ObjectType TypeName
    deriving (Show, Eq)

data BinOp = Add
           | Sub
           | Xor
           | Mul
           | Div
           | Mod
           | BitAnd
           | BitOr
           | And
           | Or
           | Lt
           | Gt
           | Eq
           | Neq
           | Lte
           | Gte
    deriving (Show, Eq, Enum)

data ModOp = ModAdd
           | ModSub
           | ModXor
    deriving (Show, Eq, Enum)

{-- Generic AST Definitions --}

--Expressions
data GExpr t = Constant Integer
             | Variable t
             | Nil
             | Binary BinOp (GExpr t) (GExpr t)
    deriving (Show, Eq)

--Statements
data GStmt t = Assign t ModOp (GExpr t)
             | Swap t t
             | Conditional (GExpr t) [GStmt t] [GStmt t] (GExpr t)
             | Loop (GExpr t) [GStmt t] [GStmt t] (GExpr t)
             | ObjectBlock TypeName t [GStmt t]
             | LocalCall MethodName [t]
             | LocalUncall MethodName [t]
             | ObjectCall t MethodName [t]
             | ObjectUncall t MethodName [t]
             | LocalBlock t (GExpr t) [GStmt t] (GExpr t)
             | Skip
    deriving (Show, Eq)

--Field/Parameter declarations
data GDecl t = GDecl DataType t
    deriving (Show, Eq)

--Method: Name, parameters, body
data GMDecl t = GMDecl MethodName [GDecl t] [GStmt t]
    deriving (Show, Eq)

--Class: Name, base class, fields, methods
data GCDecl t = GCDecl TypeName (Maybe TypeName) [GDecl t] [GMDecl t]
    deriving (Show, Eq)

--Program
data GProg t = GProg [GCDecl t]
    deriving (Show, Eq)

{-- Specific AST Definitions --}

--Plain AST
type Identifier = String
type Expression = GExpr Identifier
type Statement = GStmt Identifier
type VariableDeclaration = GDecl Identifier
type MethodDeclaration = GMDecl Identifier
type ClassDeclaration = GCDecl Identifier
type Program = GProg Identifier

--Scope-annotated AST
type SIdentifier = Integer
type SExpression = GExpr SIdentifier
type SStatement = GStmt SIdentifier
type SVariableDeclaration = GDecl SIdentifier
type SMethodDeclaration = GMDecl SIdentifier
type SClassDeclaration = GCDecl SIdentifier
type SProgram = GProg SIdentifier

{-- Other Definitions --}

type SymbolTable = Map SIdentifier VariableDeclaration
type Scope = Map Identifier SIdentifier

printAST :: (Show t) => t -> String
printAST = ppShow