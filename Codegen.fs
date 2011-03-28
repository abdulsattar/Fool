module Codegen
open Ast
open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic

type SymbolTable = { this:IDictionary<string, LocalBuilder>; mutable parent:SymbolTable option }

let generate(p:Prog, fileName) =
    
    let fn = fileName + ".exe"
    let name = new AssemblyName(fn)
    let asmb = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.Save)
    let modb = asmb.DefineDynamicModule(fn)
    let typeb = modb.DefineType(fileName)
    let methb = typeb.DefineMethod("Main", MethodAttributes.Static, typeof<Void>, Type.EmptyTypes)
    let il = methb.GetILGenerator()
    let st = ref {this = new System.Collections.Generic.Dictionary<string, LocalBuilder>(); parent = None }

    let rec gProg (p:Prog) =
        match p with StmtsProg(s) -> gStmts(s)

    and gBlock (b:Block) =
        il.BeginScope()
        st := {this = new Dictionary<string, LocalBuilder>(); parent = Some(!st) }
        match b with StmtsBlock(s) -> gStmts(s)
        il.EndScope()
        st := match (!st).parent with Some(a) -> a | None -> !st

    and gStmts (s:Stmts) =
        match s with StmtsList(sl) -> for st in sl do gStmt(st)

    and gStmt s =
        let declareLocal i t =
            let s = match i with Identifier(s) -> s
            if((!st).this.ContainsKey(s)) then failwithf "Cannot redeclare variable %s" s
            (!st).this.Add(s, il.DeclareLocal(t))
        match s with
        | DeclStmt(t, i, v) -> let t = gType t
                               declareLocal i t
        | ReadStmt(i) -> let id = gIdentifier i
                         let s = System.Console.ReadLine()
                         ()

        | WriteStmt(v) -> let t = gValue v
                          il.Emit(OpCodes.Call, typeof<System.Console>.GetMethod("Write", [| t |]))
        | BlockStmt(b) -> gBlock b
        | ValueStmt(v) -> gValue v |> ignore

    and gAssign i v =
        let t = gValue v
        let id = gIdentifier i
        if t <> id.LocalType then failwithf "%A is not compatible with %A" t id.LocalType
        il.Emit(OpCodes.Stloc, id)
        il.Emit(OpCodes.Ldloc, id)
        id.LocalType

    and gType t =
        match t with
        | IntType -> typeof<int>
        | FloatType -> typeof<float>
        | CharType -> typeof<char>
        | StringType -> typeof<char>
        | BoolType -> typeof<bool>
                         
    and gValue v : System.Type =
        match v with
        | AssignValue(i,v) -> gAssign i v
        | IdentifierValue(i) -> let id = gIdentifier i
                                il.Emit(OpCodes.Ldloc, gIdentifier i)
                                id.LocalType
        | LiteralValue(l) -> gLiteral(l)

    and gLiteral l : System.Type = 
        match l with
        | IntLit(l) -> il.Emit(OpCodes.Ldc_I4, l)
                       typeof<int>
        | FloatLit(l) -> il.Emit(OpCodes.Ldc_R8, l)
                         typeof<float>
        | CharLit(l) -> il.Emit(OpCodes.Ldc_I4, int(l))
                        typeof<char>
        | StringLit(l) -> il.Emit(OpCodes.Ldstr, l)
                          typeof<string>
        | BoolLit(b) -> gBoolLiteral b

    and gBoolLiteral b : System.Type =
        match b with
        | TrueBool -> il.Emit(OpCodes.Ldc_I4_1)
        | FalseBool -> il.Emit(OpCodes.Ldc_I4_0)
        typeof<bool>


//    and gStmt (s:Stmt) =
//
//        match s with
//        | DeclStmt(t,il) -> gIdentList il t
//        | AssignStmt(aS) -> gAssign(aS)
//        | IfStmt(c, ib, eb) ->
//            let elab = il.DefineLabel()
//            let endlab = il.DefineLabel()
//            gBool(c) |> ignore
//            il.Emit(OpCodes.Brfalse, elab)
//            gBlock(ib)
//            il.Emit(OpCodes.Br, endlab)
//            il.MarkLabel(elab)
//            gBlock(eb)
//            il.MarkLabel(endlab)
//        | WhileStmt(c, b) ->
//            let startl = il.DefineLabel()
//            let endl = il.DefineLabel()
//            il.MarkLabel(startl)
//            gBool(c) |> ignore
//            il.Emit(OpCodes.Brfalse, endl)
//            gBlock(b)
//            il.Emit(OpCodes.Br, startl)
//            il.MarkLabel(endl)
//        | ForToStmt(a, e, b) ->
//            gAssign(a)
//            let id = (match a with AssignValue(i,_) -> i)
//            let iExpr = TermExpr(FactorTerm(IntFactor(IdentInt(id))))
//            let c = BoolRelExpr(RelExpr(iExpr, e, Lte))
//            let incStmt = AssignStmt(AssignValue(id, ExprValue(Plus(TermExpr(FactorTerm(IntFactor(IdentInt(id)))), FactorTerm(IntFactor(IntLit(1)))))))
//            let incBlock = match b with StmtsBlock(s) -> match s with StmtsList(sl) -> StmtsBlock(StmtsList(sl @ [incStmt]))
//            let wStmt = WhileStmt(c, incBlock)
//            gStmt(wStmt)
//        | ForDownToStmt(a, e, b) ->
//            gAssign(a)
//            let id = (match a with AssignValue(i,_) -> i)
//            let iExpr = TermExpr(FactorTerm(IntFactor(IdentInt(id))))
//            let toExpr = e
//            let c = BoolRelExpr(RelExpr(iExpr, toExpr, Gte))
//            let decStmt = AssignStmt(AssignValue(id, ExprValue(Minus(TermExpr(FactorTerm(IntFactor(IdentInt(id)))), FactorTerm(IntFactor(IntLit(1)))))))
//            let incBlock = match b with StmtsBlock(s) -> match s with StmtsList(sl) -> StmtsBlock(StmtsList(sl @ [decStmt]))
//            let wStmt = WhileStmt(c, incBlock)
//            gStmt(wStmt)
//        | ForStmt(i,c,inc,b) ->
//            gAssign(i)
//            let block = match b with StmtsBlock(s) -> match s with StmtsList(sl) -> StmtsBlock(StmtsList(sl @ [AssignStmt(inc)]))
//            gStmt(WhileStmt(c,block))
//        | ReadStmt(i) -> il.Emit(OpCodes.Call, typeof<Console>.GetMethod("ReadLine"))
//                         il.Emit(OpCodes.Call, typeof<Int32>.GetMethod("Parse", [|typeof<string>|]))
//                         il.Emit(OpCodes.Stloc, gIdentifier(i))
//        | WriteStmt(v) -> let t = gValue(v)
//                          il.Emit(OpCodes.Call, typeof<Console>.GetMethod("Write", [|t|]))
//        | BlockStmt(b) -> gBlock(b)
//
//        ()
//    
//    and gIdentList (i:IdentList) t =
//        let tp = match t with
//                 | IntType -> typeof<int>
//                 | FloatType -> typeof<float>
//                 | CharType -> typeof<char>
//                 | StringType -> typeof<string>
//                 | BoolType -> typeof<bool>
//        match i with IdentList(il) -> for id in il do gIdentDeclOptAssign id tp
//        ()
//
//    and gIdentDeclOptAssign (i:IdentDeclOptAssign) (t:Type) =
//        let declareLocal id t = match id with Identifier(s) -> (!st).this.Add(s, il.DeclareLocal(t))
//        match i with
//        | Assign(a) -> match a with AssignValue(i, _) -> declareLocal i t
//                       gAssign(a)
//        | Ident(i) -> declareLocal i t
//        ()
//
//    and gAssign (a:Assign) =
//        match a with
//        | AssignValue(i,v) -> let t = gValue(v)
//                              let id = gIdentifier(i)
//                              if t<>id.LocalType then failwithf "Cannot assign \"{0}\" to \"{1}\" which is \"{2}\"" t i id.LocalType
//                              il.Emit(OpCodes.Stloc, id)
//    and gExpr (e:Expr) : Type =
//        match e with
//        | Plus(e,t) -> gExpr(e) |> ignore
//                       gTerm(t) |> ignore
//                       il.Emit(OpCodes.Add)
//        | Minus(e,t) -> gExpr(e) |> ignore
//                        gTerm(t) |> ignore
//                        il.Emit(OpCodes.Sub)
//        | TermExpr(t) -> gTerm(t) |> ignore
//
//        typeof<int>
//
//    and gTerm (t:Term) : Type =
//        match t with
//        | Times(t,f) -> gTerm(t) |> ignore
//                        gFactor(f) |> ignore
//                        il.Emit(OpCodes.Mul)
//        | Divide(t,f) -> gTerm(t) |> ignore
//                         gFactor(f) |> ignore
//                         il.Emit(OpCodes.Div)
//        | FactorTerm(f) -> gFactor(f) |> ignore
//
//        typeof<int>
//
//    and gFactor (f:Factor) : Type =
//        match f with
//        | IntFactor(i) -> gInt(i)
//        | ParenExpr(e) -> gExpr(e)
//
//    and gRelExpr (r:RelExpr) =
//        match r with RelExpr(e1, e2, r) -> gExpr(e1)|>ignore;gExpr(e2)|>ignore;gRelOp(r)
//
//    and gRelOp (r:RelOp) =
//        match r with
//        | Eq -> il.Emit(OpCodes.Ceq)
//        | Neq -> il.Emit(OpCodes.Ceq);il.Emit(OpCodes.Ldc_I4_0);il.Emit(OpCodes.Ceq);
//        | Gt -> il.Emit(OpCodes.Cgt)
//        | Lt -> il.Emit(OpCodes.Clt)
//        | Gte -> il.Emit(OpCodes.Clt);il.Emit(OpCodes.Ldc_I4_0);il.Emit(OpCodes.Ceq);
//        | Lte -> il.Emit(OpCodes.Cgt);il.Emit(OpCodes.Ldc_I4_0);il.Emit(OpCodes.Ceq);
//
//    and gValue (v:Ast.Value) =
//        match v with
//        | ExprValue(e) -> gExpr(e)
//        | FloatValue(f) -> gFloat(f)
//        | CharValue(c) -> gChar(c)
//        | StringValue(s) -> gString(s)
//        | BoolValue(b) -> gBool(b)
//
//    and gInt (i:Int) : Type =
//        match i with
//        | IdentInt(i) -> let id = gIdentifier(i)
//                         il.Emit(OpCodes.Ldloc, id)
//        | IntLit(i) -> il.Emit(OpCodes.Ldc_I4, i)
//
//        typeof<int>
//
//    and gFloat (f:Float) =
//        match f with
//        | IdentFloat(f) -> let id = gIdentifier(f)
//                           il.Emit(OpCodes.Ldloc, id)
//        | FloatLit(f) -> il.Emit(OpCodes.Ldc_R8, f)
//
//        typeof<float>
//
//    and gChar (c:Ast.Char) =
//        match c with
//        | IdentChar(c) -> let id = gIdentifier(c)
//                          il.Emit(OpCodes.Ldloc, id)
//        | CharLit(c) -> il.Emit(OpCodes.Ldc_I4, int(c))
//
//        typeof<char>
//
//    and gString (s:Ast.String) : Type =
//        match s with
//        | IdentString(i) -> let id = gIdentifier(i)
//                            il.Emit(OpCodes.Ldloc, id)
//        | StringLit(s) -> il.Emit(OpCodes.Ldstr, s)
//
//        typeof<string>
//
//    and gBool (b:Ast.Bool) : Type =
//        match b with
//        | BoolRelExpr(e) -> gRelExpr(e)
//        | IdentBool(i) -> let id = gIdentifier(i)
//                          il.Emit(OpCodes.Ldloc, id)
//        | TrueBool -> il.Emit(OpCodes.Ldc_I4_1)
//        | FalseBool -> il.Emit(OpCodes.Ldc_I4_0)
//
//        typeof<bool>
//
//
    and gIdentifier (i:Identifier) : LocalBuilder =
        let id = match i with Identifier(s) -> s
        let rec findIdent st = 
            if st.this.ContainsKey(id) then st.this.[id]
            else match st.parent with
                    | Some(p) -> findIdent(p)
                    | None    -> failwith "Identifier not declared"
        findIdent !st

    gProg(p)

    il.Emit(OpCodes.Ret)
    typeb.CreateType() |> ignore
    modb.CreateGlobalFunctions()
    asmb.SetEntryPoint(methb)
    asmb.Save(fn)