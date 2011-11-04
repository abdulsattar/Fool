module Codegen
open Ast
open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open System.Linq

type Variable =
    | LocalVariable of LocalBuilder
    | Parameter of string * System.Type * int
    | Field of FieldInfo

    member x.getType () =
        match x with
        | LocalVariable(l) -> l.LocalType
        | Parameter(_,t,_) -> t
        | Field(f) -> f.FieldType

type SymbolTable = { this:IDictionary<string, Variable>; mutable parent:SymbolTable option }
let generateAnonymName =
    let inc = ref 1;
    (fun () -> sprintf "<>Anonym%d" (!inc))
let anonym = ref false;

let generate(p:Prog, fileName) =

    let fn    = fileName + ".exe"
    let name  = new AssemblyName(fn)
    let asmb  = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.Save)
    let modb  = asmb.DefineDynamicModule(fn)
    let namespaces = new List<string>()

    let rec gProg (p:Prog) =
        match p with TypesProg(s) -> for c in s do gType(c)

    and gType t =
        let st = { this = new System.Collections.Generic.Dictionary<string, Variable>() ; parent = None }
        match t with
        | TypeStmtsType(i, parents, l) ->
            let parents = [| for parent in parents do yield resolveType parent |]
            let interfaces, types = parents |> Array.partition (fun x -> x.IsInterface)
            let parentType = match types.Length with
                             | 0 -> null
                             | 1 -> types.[0]
                             | _ -> failwith "Cannot have more than one base type"
            let t = modb.DefineType(gClassIdentifier(i), TypeAttributes.Public, parentType, interfaces)
            for s in l do gClassStmt s t st
            t.CreateType() |> ignore
        | OpenNamespace(s) -> namespaces.Add(s.Substring(4))

    and gClassStmt c (typeb:TypeBuilder) (st:SymbolTable) =
        match c with
        | TypeConstructorStmt (name, p, stmts) ->
            if (match name with Identifier(s) -> s) <> typeb.Name then failwith "Invalid Constructor"
            let types = [| for (t,_) in p do yield resolveType t |]
            let mb = typeb.DefineMethod(".ctor", MethodAttributes.Public, typeb.GetType(), types)
            let il = mb.GetILGenerator()
            let st = { this = new System.Collections.Generic.Dictionary<string, Variable>() ; parent = Some(st) }
            gStmts stmts mb typeb st
            il.Emit(OpCodes.Call, (typeof<obj>).GetConstructor([||]))
            il.Emit(OpCodes.Ret)
        | TypeDeclStmt(t, i) -> 
            let ty = resolveType t
            let id = gFieldIdentifier i typeb
            let field = typeb.DefineField(id, ty, FieldAttributes.Public)
            st.this.Add(id, Field(field))
            ()
        | TypeMethodStmt((returnType , name), parameters, stmts) ->
            let types = [| for (t,i) in parameters do yield resolveType t |]
            let t = resolveType returnType
            let methodName = gMethodIdentifier name typeb
            let mb = if methodName = "Main" then
                        let a = typeb.DefineMethod(methodName, MethodAttributes.Static ||| MethodAttributes.Public, t, types)
                        asmb.SetEntryPoint(a)
                        a
                     else
                        typeb.DefineMethod(methodName, MethodAttributes.Public, t, types)
            let st = { this = new System.Collections.Generic.Dictionary<string, Variable>() ; parent = Some(st) }
            let l = types.Length - 1
            for i=0 to l do
                let id = (match snd parameters.[i] with Identifier(s) -> s)
                mb.DefineParameter(i+1, ParameterAttributes.None, id) |> ignore
                st.this.Add(id, Parameter(id, types.[i], i+1))
            gStmts stmts mb typeb st
            let il = mb.GetILGenerator()
            il.Emit(OpCodes.Ret)
            ()

    and gBlock (b:Block) (mb:MethodBuilder) (t:TypeBuilder) (st:SymbolTable) =
        let il = mb.GetILGenerator()
        il.BeginScope()
        let newst = {this = new Dictionary<string, Variable>(); parent = Some(st) }
        match b with StmtsBlock(s) -> gStmts s mb t st
        il.EndScope()

    and gStmts (s:Stmts) (mb:MethodBuilder) (t:TypeBuilder) (st:SymbolTable) =
        match s with StmtsList(sl) -> for s in sl do gStmt s mb t st

    and gStmt s (mb:MethodBuilder) (t:TypeBuilder) (st:SymbolTable) =
        let il = mb.GetILGenerator()
        let declareLocal i t =
            let s = match i with Identifier(s) -> s
            if(st.this.ContainsKey(s)) then failwithf "Cannot redeclare variable %s" s
            st.this.Add(s, LocalVariable(il.DeclareLocal(t)))
        match s with
        | DeclStmt(tp, i, v) ->
            let ty = resolveType tp
            declareLocal i ty
            gAssign i v il t st |> ignore
        | AssignStmt(i,v) -> gAssign i v il t st
        | ReadStmt(i) ->
            let id = gIdentifier i il t st
            il.Emit(OpCodes.Call, typeof<System.Console>.GetMethod("ReadLine"))
            match id.getType() with
            | _ when id.getType() = typeof<int>    -> il.Emit(OpCodes.Call, typeof<System.Int32>.GetMethod("Parse", [| typeof<string> |]))
            | _ when id.getType() = typeof<float>  -> il.Emit(OpCodes.Call, typeof<System.Double>.GetMethod("Parse", [| typeof<string> |]))
            | _ when id.getType() = typeof<char>   -> il.Emit(OpCodes.Call, typeof<System.Char>.GetMethod("Parse"))
            | _ when id.getType() = typeof<bool>   -> il.Emit(OpCodes.Call, typeof<System.Boolean>.GetMethod("Parse"))
            | _ when id.getType() = typeof<string> -> ()
            | _                                    -> failwith "Invalid READ statement"
            match id with
            | LocalVariable(l) -> il.Emit(OpCodes.Stloc, l)
            | Parameter(_,_,p) -> il.Emit(OpCodes.Starg, p)
            | Field(f) -> il.Emit(OpCodes.Stfld, f)
            ()

        | WriteStmt(v) ->
            let t = gValue v il t st
            il.Emit(OpCodes.Call, typeof<System.Console>.GetMethod("Write", [| t |]))
        | BlockStmt(b) -> gBlock b mb t st
        | ValueStmt(v) -> gValue v il t st |> ignore
        | ReturnStmt(v) -> match v with
                           | Some(value) ->                            
                                   let t = gValue value il t st
                                   if isAssignable mb.ReturnType t then il.Emit(OpCodes.Ret)
                                   else failwith "Invalid return statement"
                           | None -> 
                                   if mb.ReturnType = typeof<System.Void> then il.Emit(OpCodes.Ret)
                                   else failwith "Invalid return statement"
        | WhileStmt(v,b) -> let startLabel = il.DefineLabel()
                            il.MarkLabel(startLabel)
                            let typeV = gValue v il t st
                            if typeV <> typeof<bool> then failwith "Expected System.Boolean in While Statement condition"
                            let endLabel = il.DefineLabel()
                            il.Emit(OpCodes.Brfalse, endLabel)
                            gBlock b mb t st
                            il.Emit(OpCodes.Br, startLabel)
                            il.MarkLabel(endLabel)
                            
        | ForToStmt(i, fromValue, toValue, stmts) -> declareLocal i typeof<int>
                                                     gAssign i fromValue il t st |> ignore
                                                     il.BeginScope()
                                                     let newst = {this = new Dictionary<string, Variable>(); parent = Some(st) }
                                                     let id = gIdentifier i il t st
                                                     let local = match id with LocalVariable(l) -> l | _ -> failwith "error"
                                                     let label = il.DefineLabel()
                                                     il.MarkLabel(label)
                                                     gStmts stmts mb t st
                                                     il.Emit(OpCodes.Ldloc, local)
                                                     il.Emit(OpCodes.Ldc_I4_1)
                                                     il.Emit(OpCodes.Add)
                                                     il.Emit(OpCodes.Stloc, local)
                                                     gValue toValue il t newst |> ignore
                                                     il.Emit(OpCodes.Ldloc, local)
                                                     il.Emit(OpCodes.Cgt, label)
                                                     il.Emit(OpCodes.Ldc_I4_0)
                                                     il.Emit(OpCodes.Ceq)
                                                     il.Emit(OpCodes.Brfalse, label)
                                                     il.EndScope()
        | IfStmt(ifst) -> gIfStmt (ifst) mb t st

    and gIfStmt ifst (mb:MethodBuilder) (t:TypeBuilder) (st:SymbolTable) =
        let il = mb.GetILGenerator()
        let emitElif v b (l:Label)=
            let typeV = gValue v il t st
            if typeV <> typeof<bool> then failwith "Expected System.Boolean in if statement"
            let skipThis = il.DefineLabel()
            il.Emit(OpCodes.Brfalse, skipThis)
            gBlock b mb t st
            il.Emit(OpCodes.Br, l)
            il.MarkLabel(skipThis)

        match ifst with
        | If(v,b,elifs) -> let label = il.DefineLabel()
                           emitElif v b label
                           for (v,b) in elifs do emitElif v b label
                           il.MarkLabel(label)
        | IfElse(v,b,elifs,e) -> let label = il.DefineLabel()
                                 emitElif v b label
                                 for (v,b) in elifs do emitElif v b label
                                 gBlock e mb t st
                                 il.MarkLabel(label)
         
    and gAssign i v (il:ILGenerator) (t:TypeBuilder) (st:SymbolTable) =
        let ty = gValue v il t st
        let id = gIdentifier i il t st
        if ty <> id.getType() then failwithf "%A is not compatible with %A" t (id.getType())
        match id with
        | LocalVariable(l) ->   il.Emit(OpCodes.Stloc, l)
        | Parameter(_,_,p) ->   il.Emit(OpCodes.Starg, p)
        | Field(f) -> il.Emit(OpCodes.Stfld, f)

    and gValue v (il:ILGenerator) (t:TypeBuilder) (st:SymbolTable): System.Type =
        match v with
        | IdentifierValue(i) ->
            let id = gIdentifier i il t st
            match id with
            | LocalVariable(l) -> il.Emit(OpCodes.Ldloc, l)
            | Parameter(_,_,p) -> il.Emit(OpCodes.Ldarg, p)
            | Field(f)         -> il.Emit(OpCodes.Ldfld, f)
            id.getType()
        | LiteralValue(l) -> gLiteral l il t st
        | ConstructorValue(i,l) -> let typeToConstruct = resolveType i
                                   let types = [| for ty in l do yield gValue ty il t st |]
                                   il.Emit(OpCodes.Newobj, typeToConstruct.GetConstructor(types))
                                   typeToConstruct
        | MemberCallValue(v, i, l) -> let types = [| for ty in l do yield gValue ty il t st |]
                                      let methodName = match i with Identifier(s) -> s
                                      let methodInfo = 
                                          let forOthers = 
                                            (fun () ->
                                              let typeOfValue = gValue v il t st
                                              let methodInfo = typeOfValue.GetMethod(methodName, types)
                                              methodInfo
                                            )
                                          match v with
                                          | IdentifierValue(i) -> try
                                                                    let typ = resolveType i
                                                                    let meth = typ.GetMethod(methodName, types)
                                                                    if meth.IsPublic && meth.IsStatic then meth
                                                                    else failwith "Not a public static method"
                                                                  with _ -> forOthers()
                                          | _ -> forOthers()
                                      let objectParams = methodInfo.GetParameters() |> Seq.filter (fun x -> x.ParameterType = typeof<obj>)
                                      let objectParamsIds = objectParams.Select(fun (x:ParameterInfo)->x.Position)
                                      l |> List.iteri (fun i x -> let typ = gValue x il t st
                                                                  if Seq.exists (fun y -> y = i) objectParamsIds then gBox typ il t st)
                                      il.Emit(OpCodes.Call, methodInfo)
                                      methodInfo.ReturnType
        | MemberValue(v, i) -> let ty = gValue v il t st
                               let id = match i with Identifier(s) -> s
                               let field = ty.GetField(id)
                               il.Emit(OpCodes.Ldfld, field)
                               field.FieldType
        | ParenValue(v) -> gValue v il t st
        | AnonymFunction(parameters,returnType,stmts) ->
                                                anonym := true
                                                let name = generateAnonymName()
                                                let typeb = t.DefineNestedType(name + "Class")
                                                let types = [| for (t,i) in parameters do yield resolveType t |]
                                                let t = resolveType returnType
                                                let mb = typeb.DefineMethod(name + "Method", MethodAttributes.Public, t, types)
                                                let st = { this = new System.Collections.Generic.Dictionary<string, Variable>() ; parent = Some(st) }
                                                let l = types.Length - 1
                                                for i=0 to l do
                                                    let id = (match snd parameters.[i] with Identifier(s) -> s)
                                                    mb.DefineParameter(i+1, ParameterAttributes.None, id) |> ignore
                                                    st.this.Add(id, Parameter(id, types.[i], i+1))
                                                gStmts stmts mb typeb st
                                                let ilg = mb.GetILGenerator()
                                                ilg.Emit(OpCodes.Ret)
                                                anonym := false
                                                let anonymType = typeb.CreateType()
                                                il.Emit(OpCodes.Newobj, anonymType.GetConstructor([||]))
                                                il.Emit(OpCodes.Ldftn, mb)
                                                let typeName = "System." + (if t = typeof<System.Void> then "Action" else "Func") + (match types.Length with 0 -> "" | l -> (sprintf "`%d[" l) + (types |> Array.fold (fun acc next -> acc.ToString() + "," + next.ToString()) "").Substring(1) + "]")
                                                il.Emit(OpCodes.Newobj, (resolveType <| Identifier(typeName)).GetConstructor([||]))
                                                anonymType
        | BinaryOperatorValue(b, left,right) -> let tleft = gValue left il t st
                                                let tright = gValue right il t st
                                                match b with
                                                | Plus -> if tleft = typeof<string> then
                                                              if tright <> typeof<string> then failwithf "Expected System.String but found %s" <| tright.ToString()
                                                              il.Emit(OpCodes.Call, typeof<string>.GetMethod("Concat", [|typeof<string>;typeof<string>|]))
                                                              tleft
                                                          else
                                                              if tright <> (try [typeof<int>;typeof<float>;typeof<char>;] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                              else il.Emit(OpCodes.Add)
                                                              tleft
                                                | Minus -> if tright <> (try [typeof<int>;typeof<float>;typeof<char>;] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Sub)
                                                           tleft
                                                | Mult  -> if tright <> (try [typeof<int>;typeof<float>;] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Mul)
                                                           tleft
                                                | Div   -> if tright <> (try [typeof<int>;typeof<float>;] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Mul)
                                                           tleft
                                                | Eq    -> if tright <> (try [typeof<int>;typeof<float>;typeof<bool>] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Ceq)
                                                           typeof<bool>
                                                | Neq   -> if tright <> (try [typeof<int>;typeof<float>;typeof<bool>] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Ceq)
                                                                il.Emit(OpCodes.Ldc_I4_0)
                                                                il.Emit(OpCodes.Ceq)
                                                           typeof<bool>
                                                | Gt    -> if tright <> (try [typeof<int>;typeof<float>;] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Cgt)
                                                           typeof<bool>
                                                | Lt    -> if tright <> (try [typeof<int>;typeof<float>;] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Clt)
                                                           typeof<bool>
                                                | Gte   -> if tright <> (try [typeof<int>;typeof<float>;] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Clt)
                                                                il.Emit(OpCodes.Ldc_I4_0)
                                                                il.Emit(OpCodes.Ceq)
                                                           typeof<bool>
                                                | Lte   -> if tright <> (try [typeof<int>;typeof<float>;] |> List.find ((=) tleft) with :? KeyNotFoundException -> failwithf "Operator does not support the operand") then failwithf "Expecting %s but found %s" (tleft.ToString()) (tright.ToString())
                                                           else il.Emit(OpCodes.Cgt)
                                                                il.Emit(OpCodes.Ldc_I4_0)
                                                                il.Emit(OpCodes.Ceq)
                                                           typeof<bool>
        | UnaryOperatorValue(op, v) -> match op with Not -> let typeV = gValue v il t st
                                                            if typeV <> typeof<bool> then failwithf "Expected type bool but got type %s" <| typeV.ToString()
                                                            il.Emit(OpCodes.Ldc_I4_0)
                                                            il.Emit(OpCodes.Ceq)
                                                            typeof<bool>

    and gLiteral l (il:ILGenerator) (t:TypeBuilder) (st:SymbolTable) : System.Type = 
        match l with
        | IntLit(l) -> il.Emit(OpCodes.Ldc_I4, l) ; typeof<int>
        | FloatLit(l) -> il.Emit(OpCodes.Ldc_R8, l) ; typeof<float>
        | CharLit(l) -> il.Emit(OpCodes.Ldc_I4, int(l)) ; typeof<char>
        | StringLit(l) -> il.Emit(OpCodes.Ldstr, l) ; typeof<string>
        | BoolLit(b) -> gBoolLiteral b il t st

    and gBoolLiteral b (il:ILGenerator) (t:TypeBuilder) (st:SymbolTable) : System.Type =
        match b with
        | TrueBool  -> il.Emit(OpCodes.Ldc_I4_1)
        | FalseBool -> il.Emit(OpCodes.Ldc_I4_0)
        typeof<bool>
    
    and gBox (ty:System.Type) (il:ILGenerator) (t:TypeBuilder) (st:SymbolTable) =
        il.Emit(OpCodes.Box, ty)

    and gIdentifier (i:Identifier) (il:ILGenerator) (t:TypeBuilder) (st:SymbolTable) : Variable =
        let id = match i with Identifier(s) -> s
        let rec findIdent st = 
            if st.this.ContainsKey(id) then st.this.[id]
            else
                match st.parent with
                | Some(p) -> findIdent(p)
                | None    -> failwith "Identifier not declared"
        let ident = findIdent st
        if (!anonym) && not <| st.this.ContainsKey(id) then
            let field = t.DefineField(id, ident.getType(), FieldAttributes.Private)
            Field(field)
        else 
            ident

    and gClassIdentifier i =
        let s = match i with Identifier(s) -> s
        s

    and gFieldIdentifier i (t:TypeBuilder) =
        let i = match i with Identifier(s) -> s
        i
    
    and gMethodIdentifier i (t:TypeBuilder) =
        let i = match i with Identifier(s) -> s
        i

    and resolveType i : System.Type = 
        let name = match i with Identifier(i) -> i
        match name with
        | "int" -> typeof<int>
        | "float" -> typeof<float>
        | "char" -> typeof<char>
        | "string" -> typeof<string>
        | "bool" -> typeof<bool>
        | "void" -> typeof<System.Void>
        | "object" -> typeof<obj>
        | _ ->  let asmbs = asmb.GetReferencedAssemblies()
                let findTypeWithNamespaces (tn:string) (asmb:Assembly) =
                    let mutable returnType = None
                    for str in namespaces do
                        let t = asmb.GetType(str + "." + tn,false)
                        if t <> null then returnType <- Some(t)
                    returnType
                let rec findType (asmbs:AssemblyName[]) =
                    match asmbs.Length with
                    | 0 -> failwith "Type not found"
                    | _ ->  let asmName = asmbs.[0]
                            let asmbly = Assembly.Load(asmName)
                            let a = findTypeWithNamespaces name asmbly
                            match a with
                            | Some(t) -> t
                            | None -> failwith "Type not found"
                let fromThis = asmb.GetType(name,false)
                if fromThis <> null then fromThis
                else findType asmbs

    and isAssignable (left:System.Type) (right:System.Type) : bool =
        if left = right then true
        else right.IsSubclassOf(left)

    gProg(p)

    modb.CreateGlobalFunctions()
    asmb.Save(fn)
