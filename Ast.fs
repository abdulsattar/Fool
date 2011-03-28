module Ast
open System

type Prog = 
    | StmtsProg of Stmts

and Block =
    | StmtsBlock of Stmts

and Stmts =
    | StmtsList of Stmt list

and Stmt =
    | DeclStmt of Type * Identifier * Value
    | ReadStmt of Identifier
    | WriteStmt of Value
    | BlockStmt of Block
    | ValueStmt of Value

and Value =
    | AssignValue of Identifier * Value
    | IdentifierValue of Identifier
    | LiteralValue of Literal

and Literal =
    | IntLit of int
    | FloatLit of float
    | CharLit of char
    | StringLit of string
    | BoolLit of BoolLiteral

and BoolLiteral =
    | TrueBool
    | FalseBool

and Identifier = 
    | Identifier of string
    
and Type =
    | IntType
    | FloatType
    | CharType
    | StringType
    | BoolType
