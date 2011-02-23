module Ast
open System

type Prog = 
    | StmtsProg of Stmts

and Block =
    | StmtsBlock of Stmts

and Stmts =
    | StmtsList of Stmt list

and Stmt =
    | DeclStmt of Type * IdentList
    | AssignStmt of Assign
    | IfStmt of Bool * Block * Block
    | WhileStmt of Bool * Block
    | ForToStmt of Assign * Expr * Block
    | ForDownToStmt of Assign * Expr * Block
    | ForStmt of Assign * Bool * Assign * Block
    | ReadStmt of Identifier
    | WriteStmt of Value
    | BlockStmt of Block

and IdentList =
    | IdentList of IdentDeclOptAssign list

and IdentDeclOptAssign =
    | Assign of Assign
    | Ident of Identifier

and Assign = 
    | AssignValue of Identifier * Value

and Expr =
    | Plus of Expr * Term
    | Minus of Expr * Term
    | TermExpr of Term

and Term =
    | Times of Term * Factor
    | Divide of Term * Factor
    | FactorTerm of Factor

and Factor =
    | IntFactor of Int
    | ParenExpr of Expr

and RelExpr = 
    | RelExpr of Expr * Expr * RelOp

and RelOp =
    | Eq
    | Neq
    | Gt
    | Lt
    | Gte
    | Lte

and Value =
    | ExprValue of Expr
    | FloatValue of Float
    | CharValue of Char
    | StringValue of String
    | BoolValue of Bool

and Int =
    | IdentInt of Identifier
    | IntLit of int

and Float = 
    | IdentFloat of Identifier
    | FloatLit of float

and Char =
    | IdentChar of Identifier
    | CharLit of char

and String =
    | IdentString of Identifier
    | StringLit of string

and Bool =
    | BoolRelExpr of RelExpr
    | IdentBool of Identifier
    | TrueBool
    | FalseBool

and Type =
    | IntType
    | FloatType
    | CharType
    | StringType
    | BoolType

and Identifier = 
    | Identifier of string