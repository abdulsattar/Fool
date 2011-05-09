module Ast
open System
open System.Collections.Generic

type Prog = 
    | TypesProg of Type list

and Type =
    | TypeStmtsType of Identifier * Identifier list * TypeStmt list
    | OpenNamespace of string

and TypeStmt =
    | TypeConstructorStmt of Identifier * (Identifier * Identifier) list * Stmts
    | TypeDeclStmt of Identifier * Identifier
    | TypeMethodStmt of (Identifier * Identifier) * (Identifier * Identifier) list * Stmts

and Block =
    | StmtsBlock of Stmts

and Stmts =
    | StmtsList of Stmt list

and Stmt =
    | DeclStmt of Identifier * Identifier * Value
    | ReadStmt of Identifier
    | WriteStmt of Value
    | BlockStmt of Block
    | ReturnStmt of Value option
    | ValueStmt of Value
    | ForToStmt of Identifier * Value * Value * Stmts
    | ForDowntoStmt of Identifier * Value * Value * Stmts
    | WhileStmt of Value * Block
    | IfStmt of IfStmt

and IfStmt =
    | If of Value * Block * (Value * Block) list
    | IfElse of Value * Block * (Value * Block) list * Block

and Value =
    | AssignValue of Identifier * Value
    | IdentifierValue of Identifier
    | LiteralValue of Literal
    | ConstructorValue of Identifier * Value list
    | MemberCallValue of Value * Identifier * Value list
    | MemberValue of Value * Identifier
    | ParenValue of Value
    | AnonymFunction of (Identifier * Identifier) list * Identifier * Stmts
    | BinaryOperatorValue of BinaryOperator * Value * Value
    | UnaryOperatorValue of UnaryOperator * Value

and BinaryOperator =
    | Plus
    | Minus
    | Mult
    | Div
    | Eq
    | Neq
    | Gt
    | Lt
    | Gte
    | Lte

and UnaryOperator =
    | Not

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
