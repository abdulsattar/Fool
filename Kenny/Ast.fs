module Ast
open System

type Prog = | StmtsProg of Stmts

and Block =
    | StmtsBlock of Stmts

and Stmts =
    | StmtsList of Stmt list

and Stmt =
    | DeclStmt of IdentList
    | AssignStmt of Assign
    | IfStmt of RelExpr * Block * Block
    | WhileStmt of RelExpr * Block
    | ForToStmt of Assign * Variable * Block
    | ForDownToStmt of Assign * Variable * Block
    | ForStmt of Assign * RelExpr * Assign * Block
    | ReadStmt of Identifier
    | WriteStmt of WriteStmt
    | BlockStmt of Block

and WriteStmt =
    | WriteStr of String
    | WriteExpr of Expr

and IdentList =
    | IdentList of IdentDeclOptAssign list

and IdentDeclOptAssign =
    | Assign of Assign
    | Ident of Identifier

and Assign = 
| AssignExpr of Identifier * Expr

and Expr =
    | Plus of Expr * Term
    | Minus of Expr * Term
    | TermExpr of Term

and Term =
    | Times of Term * Factor
    | Divide of Term * Factor
    | FactorTerm of Factor

and Factor =
    | VarFactor of Variable
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

and Variable =
    | IdentVariable of Identifier
    | IntVariable of int

and String = 
| String of string

and Identifier = 
| Identifier of string