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

    let rec gProg(p:Prog) =
        match p with StmtsProg(s) -> gStmts(s)

    and gBlock(b:Block) =
        il.BeginScope()
        st := {this = new Dictionary<string, LocalBuilder>(); parent = Some(!st) }
        match b with StmtsBlock(s) -> gStmts(s)
        il.EndScope()
        st := match (!st).parent with Some(a) -> a | None -> !st

    and gStmts (s:Stmts) =
        match s with StmtsList(sl) -> for st in sl do gStmt(st)

    and gStmt (s:Stmt) =

        match s with
        | DeclStmt(il) -> gIdentList(il)
        | AssignStmt(aS) -> gAssign(aS)
        | IfStmt(c, ib, eb) ->
            let elab = il.DefineLabel()
            let endlab = il.DefineLabel()
            gRelExpr(c)
            il.Emit(OpCodes.Brfalse, elab)
            gBlock(ib)
            il.Emit(OpCodes.Br, endlab)
            il.MarkLabel(elab)
            gBlock(eb)
            il.MarkLabel(endlab)
        | WhileStmt(c, b) ->
            let startl = il.DefineLabel()
            let endl = il.DefineLabel()
            il.MarkLabel(startl)
            gRelExpr(c)
            il.Emit(OpCodes.Brfalse, endl)
            gBlock(b)
            il.Emit(OpCodes.Br, startl)
            il.MarkLabel(endl)
        | ForToStmt(a, v, b) ->
            gAssign(a)
            let id = (match a with AssignExpr(i,e) -> i)
            let iExpr = TermExpr(FactorTerm(VarFactor(IdentVariable(id))))
            let toExpr = TermExpr(FactorTerm(VarFactor(v)))
            let relExpr = RelExpr(iExpr, toExpr, Lte)
            let incStmt = AssignStmt(AssignExpr(id, Plus(TermExpr(FactorTerm(VarFactor(IdentVariable(id)))), FactorTerm(VarFactor(IntVariable(1))))))
            let incBlock = match b with StmtsBlock(s) -> match s with StmtsList(sl) -> StmtsBlock(StmtsList(sl @ [incStmt]))
            let wStmt = WhileStmt(relExpr, incBlock)
            gStmt(wStmt)
        | ForDownToStmt(a, v, b) ->
            gAssign(a)
            let id = (match a with AssignExpr(i,e) -> i)
            let iExpr = TermExpr(FactorTerm(VarFactor(IdentVariable(id))))
            let toExpr = TermExpr(FactorTerm(VarFactor(v)))
            let relExpr = RelExpr(iExpr, toExpr, Gte)
            let decStmt = AssignStmt(AssignExpr(id, Minus(TermExpr(FactorTerm(VarFactor(IdentVariable(id)))), FactorTerm(VarFactor(IntVariable(1))))))
            let incBlock = match b with StmtsBlock(s) -> match s with StmtsList(sl) -> StmtsBlock(StmtsList(sl @ [decStmt]))
            let wStmt = WhileStmt(relExpr, incBlock)
            gStmt(wStmt)
        | ForStmt(i,c,inc,b) ->
            gAssign(i)
            let block = match b with StmtsBlock(s) -> match s with StmtsList(sl) -> StmtsBlock(StmtsList(sl @ [AssignStmt(inc)]))
            gStmt(WhileStmt(c,block))
        | ReadStmt(i) -> il.Emit(OpCodes.Call, typeof<Console>.GetMethod("ReadLine"))
                         il.Emit(OpCodes.Call, typeof<Int32>.GetMethod("Parse", [|typeof<string>|]))
                         il.Emit(OpCodes.Stloc, gIdentifier(i))
        | WriteStmt(s) -> gWriteStmt(s)
        | BlockStmt(b) -> gBlock(b)

        ()

    and gWriteStmt (w:WriteStmt) =
        
        match w with 
        | WriteStr(s) -> let str = gString(s)
                         il.Emit(OpCodes.Ldstr, str)
                         il.Emit(OpCodes.Call, typeof<Console>.GetMethod("Write", [|typeof<string>|]))
        | WriteExpr(e) -> gExpr(e)
                          il.Emit(OpCodes.Call, typeof<Console>.GetMethod("Write", [|typeof<int>|]))
        
        ()
    
    and gIdentList (i:IdentList) =
        match i with IdentList(il) -> for id in il do gIdentDeclOptAssign(id)
        ()

    and gIdentDeclOptAssign (i:IdentDeclOptAssign) =
        let declareLocal id = match id with Identifier(s) -> (!st).this.Add(s, il.DeclareLocal(typeof<int>))
        match i with
        | Assign(a) -> match a with AssignExpr(i, _) -> declareLocal(i)
                       gAssign(a)
        | Ident(i) -> declareLocal(i)
        ()

    and gAssign (a:Assign) =
        match a with
        | AssignExpr(i,e) -> gExpr(e)
                             let id = gIdentifier(i)
                             il.Emit(OpCodes.Stloc, id)

    and gExpr (e:Expr) =
        match e with
        | Plus(e,t) -> gExpr(e)
                       gTerm(t)
                       il.Emit(OpCodes.Add)
        | Minus(e,t) -> gExpr(e)
                        gTerm(t)
                        il.Emit(OpCodes.Sub)
        | TermExpr(t) -> gTerm(t)

    and gTerm (t:Term) =
        match t with
        | Times(t,f) -> gTerm(t)
                        gFactor(f)
                        il.Emit(OpCodes.Mul)
        | Divide(t,f) -> gTerm(t)
                         gFactor(f)
                         il.Emit(OpCodes.Div)
        | FactorTerm(f) -> gFactor(f)

    and gFactor (f:Factor) =
        match f with
        | VarFactor(v) -> gVariable(v)
        | ParenExpr(e) -> gExpr(e)

    and gRelExpr (r:RelExpr) =
        match r with RelExpr(e1, e2, r) -> gExpr(e1);gExpr(e2);gRelOp(r)

    and gRelOp (r:RelOp) =
        match r with
        | Eq -> il.Emit(OpCodes.Ceq)
        | Neq -> il.Emit(OpCodes.Ceq);il.Emit(OpCodes.Ldc_I4_0);il.Emit(OpCodes.Ceq);
        | Gt -> il.Emit(OpCodes.Cgt)
        | Lt -> il.Emit(OpCodes.Clt)
        | Gte -> il.Emit(OpCodes.Clt);il.Emit(OpCodes.Ldc_I4_0);il.Emit(OpCodes.Ceq);
        | Lte -> il.Emit(OpCodes.Cgt);il.Emit(OpCodes.Ldc_I4_0);il.Emit(OpCodes.Ceq);

    and gVariable (v:Variable) =
        match v with
        | IdentVariable(i) -> let id = gIdentifier(i)
                              il.Emit(OpCodes.Ldloc, id)
        | IntVariable(i) -> il.Emit(OpCodes.Ldc_I4, i)

    and gString (s:Ast.String) : string =
        match s with String(str) -> str

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
    
    ()