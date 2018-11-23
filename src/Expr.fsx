#load "load.fsx"

open Aardvark.Base
open Aardvark.Base.Monads.State


[<AutoOpen>]
module rec Stuff =

    type ExternalTypeInfo =
        {
            eassembly   : string
            enamespace  : list<string>
            ename       : string
            egeneric    : list<TypeRef>
        }

        static member Top  =
            {
                eassembly = ""
                enamespace = []
                ename = "Entry"
                egeneric = []
            }

        member x.efullname = 
            x.enamespace @ [x.ename] |> String.concat "."

    type UnionCase =
        {
            uname       : string
            ufields     : list<string * TypeRef>
        }

    type TypeDefInfo =
        | DRecord of fields : list<string * TypeRef> 
        | DUnion of cases : list<UnionCase>
        | DObject of fields : list<string * TypeRef>

    type TypeDef =
        {
            dinfo   : ExternalTypeInfo
            ddef    : TypeDefInfo
        }

    module TypeDef =
        let toString (d : TypeDef) =
            let name = d.dinfo.ename
            match d.ddef with
            | DRecord(fields) ->
                fields
                |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                |> String.concat "; "
                |> sprintf "type %s = { %s }" name

            | DUnion(cases) ->
                let cases = 
                    cases |> List.map (fun c ->
                        c.ufields
                        |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                        |> String.concat " * "
                        |> sprintf "%s of %s" c.uname
                    )
                sprintf "type %s = %s" name (String.concat " | " cases)
            | DObject fields ->
                fields
                |> List.map (fun (name, typ) -> sprintf "val mutable public : %s : %s" name (TypeRef.toString typ))
                |> String.concat "; "
                |> sprintf "type %s = struct %s end" name
                
    type TypeRef =
        | RUnit
        | RInt of signed : bool * bits : int
        | RFloat of bits : int
        | RChar
        | RString

        | RImported of info : ExternalTypeInfo * def : Option<TypeDefInfo>
        | RTuple of elements : list<TypeRef>
        | RFunction of TypeRef * TypeRef

    module TypeRef =
        let rec toString (t : TypeRef) =
            match t with
            | RInt(false, 8) -> "byte"
            | RInt(true, 32) -> "int"
            | RFloat(64) -> "float"
            | RUnit -> "unit"

            | RInt(true, b) -> sprintf "int%d" b
            | RInt(false, b) -> sprintf "uint%d" b
            | RFloat b -> sprintf "float%d" b
            | RChar -> "char"
            | RString -> "string"

            | RTuple [e] -> toString e
            | RTuple elements -> elements |> List.map toString |> String.concat " * " |> sprintf "(%s)"
            | RImported(info,_) -> 
                match info.egeneric with
                    | [] -> info.efullname
                    | args -> sprintf "%s<%s>" info.efullname (args |> List.map toString |> String.concat ", ")
            | RFunction(a,b) -> sprintf "%s -> %s" (toString a) (toString b)

        let tryOfSimpleType =
            LookupTable.lookupTable' [
                typeof<char>, RChar
                typeof<string>, RString

                typeof<int8>, RInt(true, 8)
                typeof<uint8>, RInt(false, 8)
                typeof<int16>, RInt(true, 16)
                typeof<uint16>, RInt(false, 16)
                typeof<int32>, RInt(true, 32)
                typeof<uint32>, RInt(false, 32)
                typeof<int64>, RInt(true, 64)
                typeof<uint64>, RInt(false, 64)

                typeof<float16>, RFloat(16)
                typeof<float32>, RFloat(32)
                typeof<float>, RFloat(64)
                
                typeof<unit>, RUnit
            ]

        let tryApply (args : list<TypeRef>) (fType : TypeRef) =
            match args with
                | [] -> 
                    Some fType
                | a :: b ->
                    match fType with
                    | RFunction(h,t)
                    | RFunction(RTuple [h], t) when a = h -> tryApply b t
                    | _ -> None


    type ExternalFunctionInfo =
        {
            fdeclaring  : ExternalTypeInfo
            fname       : string
            fgeneric    : list<TypeRef>
            fparameters : list<list<string * TypeRef>>
            freturn     : TypeRef
        }

        member x.ffullname = 
            x.fdeclaring.efullname + "." + x.fname

        static member Entry(name : string, typ : TypeRef) =
            {
                fdeclaring = ExternalTypeInfo.Top
                fname = name
                fgeneric = []
                fparameters = []
                freturn = typ
            }

        member x.ftype =
            let tup = function [e] -> e | es -> RTuple es
            let types = x.fparameters |> List.map (List.map snd >> tup)
            List.foldBack (fun e s -> RFunction(e,s)) types x.freturn 
  
    module ExternalFunctionInfo =
        let toString (s : ExternalFunctionInfo) : string =
            let decl = s.fdeclaring.efullname
            let args =
                s.fparameters |> List.map (fun block ->
                    block
                    |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                    |> String.concat ", "
                    |> sprintf "(%s)"
                )
                |> String.concat " "
            sprintf "%s.%s %s : %s" decl s.fname args (TypeRef.toString s.freturn)


    type FunctionDef =
        {
            finfo       : ExternalFunctionInfo
            fexpr       : Expr
        }

    module FunctionDef =
        open Patterns
        
        let toString (d : FunctionDef) =
            match d.fexpr with
                | Lambdas(vs, b) ->
                    let b : Expr = b
                    let args =
                        d.finfo.fparameters |> List.map (fun block ->
                            block
                            |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                            |> String.concat ", "
                            |> sprintf "(%s)"
                        )
                        |> String.concat " "
                    sprintf "let %s %s : %s = %s" d.finfo.fname args (TypeRef.toString b.Type) (Expr.toString b)

                | b ->
                    sprintf "let %s : %s = %s" d.finfo.fname (TypeRef.toString b.Type) (Expr.toString b)

        let ofExpr (name : string) (ex : Expr) =
            let args, body = 
                match ex with
                    | Lambdas(vs, b) -> vs, b
                    | _ -> [], ex
            
            let info =
                {
                    fdeclaring  = ExternalTypeInfo.Top
                    fname       = name
                    fgeneric    = []
                    fparameters = args |> List.map (fun (v : Var) -> [v.Name, v.Type])
                    freturn     = body.Type
                }
            { 
                finfo = info
                fexpr = ex
            }

    type Literal =
        | LUnit
        | LString of value : string
        | LChar of value : char
        | LInt of signed : bool * bits : int * value : uint64
        | LFloat of bits : int * value : float

        member x.ltype =
            match x with
                | LUnit -> RUnit
                | LString _ -> RString
                | LChar _ -> RChar
                | LInt(s,b,_) -> RInt(s,b)
                | LFloat(b,_) -> RFloat(b)

    module Literal =
        let toString (l : Literal) =
            match l with
                | LUnit -> "()"
                | LString str -> sprintf "\"%s\"" str
                | LChar c -> sprintf "'%c'" c
                | LInt(true, 8, v) -> sprintf "%dy" (int8 v)
                | LInt(false, 8, v) -> sprintf "%duy" (uint8 v)
                | LInt(true, 16, v) -> sprintf "%ds" (int16 v)
                | LInt(false, 16, v) -> sprintf "%dus" (uint16 v)
                | LInt(true, 32, v) -> sprintf "%d" (int v)
                | LInt(false, 32, v) -> sprintf "%du" (uint32 v)
                | LInt(true, 64, v) -> sprintf "%dL" (int64 v)
                | LInt(false, 64, v) -> sprintf "%dUL" (uint64 v)

                | LFloat(32,v) -> sprintf "%ff" v
                | LFloat(64,v) -> sprintf "%f" v
                
                | LInt(true, b, v) -> sprintf "int%d(%dUL)" b v
                | LInt(false, b, v) -> sprintf "uint%d(%dUL)" b v
                | LFloat(b,v) -> sprintf "float%d(%f)" b v

        let tryOfValue (o : 'a) =
            match TypeRef.tryOfSimpleType typeof<'a> with
                | Some RChar            -> LChar (unbox<char> o) |> Some
                | Some RString          -> LString (unbox<string> o) |> Some
                | Some (RInt(s, b))     -> LInt(s, b, System.Convert.ToUInt64 (o :> obj)) |> Some
                | Some (RFloat(b))      -> LFloat(b, System.Convert.ToDouble (o :> obj)) |> Some
                | _ -> None
                
        let ofTypedValue (t : TypeRef) (o : obj) =
            match t with
                | RUnit            -> LUnit
                | RChar            -> LChar (unbox<char> o)
                | RString          -> LString (unbox<string> o)
                | (RInt(s, b))     -> LInt(s, b, System.Convert.ToUInt64 o)
                | (RFloat(b))      -> LFloat(b, System.Convert.ToDouble o)
                | _ -> failwithf "[FShade] not a primitive type: %A" (TypeRef.toString t)

        let ofValue (o : 'a) =
            match tryOfValue o with
                | Some v -> v
                | None -> failwithf "[FShade] not a primitive type: %A" typeof<'a>



    [<Struct; CustomComparison; CustomEquality>]
    type Var private(name : string, typ : TypeRef, isMutable : bool, id : int) =
        static let mutable currentId = 0

        member x.Name = name
        member x.Type = typ
        member x.IsMutable = isMutable
        member x.Id = id

        interface System.IComparable with
            member x.CompareTo o =
                match o with
                | :? Var as o -> compare id o.Id
                | _ -> failwith "uncomparable"

        override x.ToString() = name
        override x.GetHashCode() = id
        override x.Equals o =
            match o with
            | :? Var as o -> id = o.Id
            | _ -> false

        new(name : string, typ : TypeRef, isMutable : bool) = 
            Var(name, typ, isMutable, System.Threading.Interlocked.Increment(&currentId))
            
        new(name : string, typ : TypeRef) = 
            Var(name, typ, false, System.Threading.Interlocked.Increment(&currentId))

    type ExprInfo =
        | EValue of value : Literal
        | EVar of Var
        | ECall of f : ExternalFunctionInfo
        | ELambda of Var
        | ENewRecord of TypeRef
        | ESequential
        | EFieldGet of name : string * dst : TypeRef
        | EFieldSet of name : string * dst : TypeRef
        | ELet of v : Var
        | EDefault of TypeRef

    type Expr(info : ExprInfo, typ : TypeRef, children : list<Expr>) =
        member x.Info = info
        member x.Type = typ
        member x.Children = children

        static member Var(v : Var) =
            Expr(EVar v, v.Type, [])

        static member Value(value : 'a) =
            let value = Literal.ofValue value
            Expr(EValue value, value.ltype, [])
            
        static member Value(value : obj, t : TypeRef) =
            let value = Literal.ofTypedValue t value
            Expr(EValue value, value.ltype, [])

        static member Call(f : ExternalFunctionInfo, args : list<Expr>) = 
            let argTypes = args |> List.map (fun e -> e.Type)
            match TypeRef.tryApply argTypes f.ftype with
                | Some ret ->
                    Expr(ECall f, ret, args)
                | None ->
                    failwithf 
                        "[FShade] inconsistent argument types for %s: [%s]" 
                        (ExternalFunctionInfo.toString f) 
                        (argTypes |> List.map TypeRef.toString |> String.concat "; ")

        static member Lambda(v : Var, e : Expr) =
            let t = RFunction(v.Type, e.Type)
            Expr(ELambda v, t, [e])

        static member NewRecord(t : TypeRef, values : list<Expr>) =
            Expr(ENewRecord t, t, values)

        static member Sequential(l : Expr, r : Expr) =
            Expr(ESequential, r.Type, [l;r])
            
        static member FieldGet(dst : Expr, name : string, typ : TypeRef) =
            Expr(EFieldGet(name, typ), typ, [dst])

        static member FieldSet(dst : Expr, name : string, value : Expr) =
            Expr(EFieldSet(name, dst.Type), RUnit, [dst; value])

        static member Let(v : Var, value : Expr, body : Expr) =
            Expr(ELet v, body.Type, [value; body])

        static member Default(t : TypeRef) =
            Expr(EDefault t, t, [])

    module Patterns =
        let (|Var|_|) (e : Expr) =
            match e.Info with
            | EVar v -> Some v
            | _ -> None

        let (|Value|_|) (e : Expr) =
            match e.Info with
            | EValue v -> Some v
            | _ -> None
            
        let (|Call|_|) (e : Expr) =
            match e.Info with
            | ECall v -> Some(v, e.Children)
            | _ -> None
            
        let (|Lambda|_|) (e : Expr) =
            match e.Info with
            | ELambda v -> Some(v, List.head e.Children)
            | _ -> None
            
        let (|Lambdas|_|) (e : Expr) =
            match e with
                | Lambda(v,b) ->
                    match b with
                        | Lambdas(vs, b) -> Some (v :: vs, b)
                        | _ -> Some([v], b)
                | _ ->
                    None
                    
        let (|NewRecord|_|) (e : Expr) =
            match e.Info with
                | ENewRecord(t) -> Some(t, e.Children)
                | _ -> None
                
        let (|Sequential|_|) (e : Expr) =
            match e.Info with
                | ESequential -> Some(List.head e.Children, List.item 1 e.Children)
                | _ -> None

        let (|FieldSet|_|) (e : Expr) =
            match e.Info with
                | EFieldSet(name,_) -> Some(List.head e.Children, name, List.item 1 e.Children)
                | _ -> None
        
        let (|FieldGet|_|) (e : Expr) =
            match e.Info with
                | EFieldGet(name,_) -> Some(List.head e.Children, name)
                | _ -> None
        
        
        let (|Let|_|) (e : Expr) =
            match e.Info with
                | ELet v -> Some(v, List.head e.Children, List.item 1 e.Children)
                | _ -> None
        
        let (|Default|_|) (e : Expr) =
            match e.Info with
                | EDefault t -> Some(t)
                | _ -> None
        
    module Expr =
        open Patterns

        let rec toString (e : Expr) =
            match e with
                | Value l -> Literal.toString l
                | Var v -> v.Name
                | Call(f, args) -> 
                    let rec zipper (l : list<'a>) (r : list<'b>) =
                        match l, r with
                            | l :: ls, r :: rs -> 
                                let (res, rest) = zipper ls rs
                                (l,r) :: res, rest
                            | [], rest ->
                                [], rest

                            | _, [] -> failwith "bad"

                    let rec callArgs (p : list<list<string * TypeRef>>) (args : list<string>) =
                        match p with
                            | [] -> []
                            | h :: ps ->
                                let tup, args = zipper h args

                                let mine = 
                                    match tup with
                                        | [_,e] -> e
                                        | _ -> tup |> List.map snd |> String.concat ", " |> sprintf "(%s)"

                                let rest = callArgs ps args
                                mine :: rest

                    let exprs = args |> List.map toString
                    let args = callArgs f.fparameters exprs
                    
                    sprintf "(%s %s)" (f.ffullname) (String.concat " " args)
                | Lambdas(vs, b) ->
                    let vs = vs |> List.map (fun v -> sprintf "(%s : %s)" v.Name (TypeRef.toString v.Type)) |> String.concat " "
                    sprintf "fun %s -> %s" vs (toString b)
                
                | NewRecord(t, values) ->
                    match t with
                        | RImported(info, Some (DRecord fields)) ->  
                            List.zip fields values |> List.map (fun ((f,_),v) ->
                                sprintf "%s = %s" f (toString v)
                            )
                            |> String.concat "; "
                            |> sprintf "{ %s }"
                        | _ ->
                            failwith "not a record"

                | Sequential(l,r) ->
                    sprintf "%s; %s" (toString l) (toString r)

                | FieldSet(l,n,v) ->
                    sprintf "%s.%s <- %s" (toString l) n (toString v)
                    
                | FieldGet(l,n) ->
                    sprintf "%s.%s" (toString l) n

                | Let(v,e,b) ->
                    if v.IsMutable then
                        sprintf "let mutable %s = %s in %s" v.Name (toString e) (toString b)
                    else 
                        sprintf "let %s = %s in %s" v.Name (toString e) (toString b)

                | Default t ->
                    sprintf "default<%s>" (TypeRef.toString t)


                | _ ->
                    failwith "unreachable"


    type Module =
        {
            mname       : string
            mtypes      : Map<ExternalTypeInfo, TypeDef>
            mfunctions  : Map<ExternalFunctionInfo, FunctionDef>
        }

    module Module =
        let toString (m : Module) =
            let all =
                seq {
                    yield sprintf "module %s = " m.mname
                    yield! m.mtypes |> Map.toSeq |> Seq.map (snd >> TypeDef.toString >> sprintf "    %s")
                    yield! m.mfunctions |> Map.toSeq |> Seq.map (snd >> FunctionDef.toString >> sprintf "    %s")
                }
            String.concat "\r\n" all

module rec AdapterStuff =
    open System
    open System.Reflection
    open Stuff
    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    type private TExpr = Stuff.Expr
    type private TVar = Stuff.Var

    type TranslationState =
        {
            typeImps    : MapExt<string, Set<ExternalTypeInfo>>
            typeDefs    : hmap<Type, TypeDef>
            typeRefs    : hmap<Type, TypeRef>
            
            funImps     : MapExt<string, Set<ExternalFunctionInfo>>
            funRefs     : hmap<MethodBase, ExternalFunctionInfo>
            funDefs     : hmap<MethodBase, FunctionDef>

            variables   : MapExt<Var, TVar>
        }

        static member Empty = { typeImps = MapExt.empty; typeRefs = HMap.empty; typeDefs = HMap.empty; funImps = MapExt.empty; funRefs = HMap.empty; funDefs = HMap.empty; variables = MapExt.empty }

    module TranslationState =
        let importType (t : ExternalTypeInfo) (s : TranslationState) =
            { s with
                typeImps = MapExt.alter t.eassembly (Option.defaultValue Set.empty >> Set.add t >> Some) s.typeImps
            }

        let importFunction (t : ExternalFunctionInfo) (s : TranslationState) =
            let ass = t.fdeclaring.eassembly
            { s with
                funImps = MapExt.alter ass (Option.defaultValue Set.empty >> Set.add t >> Some) s.funImps
            }

        let defineType (t : Type) (def : TypeDef) (s : TranslationState) =
            { s with
                typeDefs = HMap.add t def s.typeDefs
            }
            
        let defineFunction (t : MethodBase) (def : FunctionDef) (s : TranslationState) =
            { s with
                funDefs = HMap.add t def s.funDefs
            }
        let declare (t : Var) (def : TVar) (s : TranslationState) =
            { s with
                variables = MapExt.add t def s.variables
            }

        let toModule (s : TranslationState) =
            {
                mname = ExternalTypeInfo.Top.efullname
                mfunctions = s.funDefs |> HMap.toSeq |> Seq.map (fun (_,d) -> d.finfo, d) |> Map.ofSeq
                mtypes = s.typeDefs |> HMap.toSeq |> Seq.map (fun (_,d) -> d.dinfo, d) |> Map.ofSeq
            }
            


    module ExternalTypeInfo =
        open System.Text.RegularExpressions

        let private genericRx = Regex @"^(.*?)`([0-9]+)"
        
        let rec private getNamespace (t : Type) =
            let td = t.DeclaringType
            if isNull td then
                if System.String.IsNullOrWhiteSpace t.Namespace || t.Namespace = "global" then
                    []
                else
                    t.Namespace.Split([| '.' |], StringSplitOptions.None) |> Array.toList
            else
                if td.IsGenericType then failwith "[FShade] declaring types may currently not be generic"
                let nd = getNamespace td
                nd @ [td.Name]

        let private (|Generic|NonGeneric|) (t : System.Type) =
            let ns = getNamespace t

            if t.IsGenericType then
                let m = genericRx.Match t.Name
                if m.Success then
                    let name = m.Groups.[1].Value
                    let cnt = m.Groups.[2].Value |> int
                    Generic(ns, name, cnt)

                else
                    NonGeneric(ns, t.Name)
            else
                NonGeneric(ns, t.Name)

        let ofTypeS (t : System.Type) =
            state {
                let ass = t.Assembly.GetName().Name

                match t with
                    | Generic(ns, name, cnt) ->
                        let! targs = t.GetGenericArguments() |> Array.toList |> List.mapS TypeRef.ofTypeS
                        let res =  
                            {
                                eassembly = ass
                                enamespace = ns
                                ename = name
                                egeneric = targs
                            }
                        do! State.modify (TranslationState.importType res)
                        return res

                    | NonGeneric(ns, name) ->
                        let res = 
                            {
                                eassembly = ass
                                enamespace = ns
                                ename = name
                                egeneric = []
                            }
                        
                        do! State.modify (TranslationState.importType res)
                        return res
            }
            
    module TypeDef =
        let tryOfTypeS (t : Type) =
            state {
                let! s = State.get
                match HMap.tryFind t s.typeDefs with
                | Some def ->
                    return Some def 
                | None -> 
                    let! ref = ExternalTypeInfo.ofTypeS t

                    if FSharpType.IsRecord(t, true) then
                        let! fields = 
                            FSharpType.GetRecordFields(t, true)
                            |> Array.toList
                            |> List.mapS (fun p ->
                                state {
                                    let! t = TypeRef.ofTypeS p.PropertyType
                                    return (p.Name, t)
                                }
                            )
                            
                        let def = { dinfo = ref; ddef = DRecord(fields) }
                        do! State.modify (TranslationState.defineType t def)
                        return Some def

                    elif FSharpType.IsUnion(t, true) then
                        let! cases =
                            FSharpType.GetUnionCases(t, true) |> Array.toList |> List.mapS (fun c ->
                                state {
                                    let! fields = 
                                        c.GetFields() 
                                        |> Array.toList
                                        |> List.mapS (fun p ->
                                            state {
                                                let! t = TypeRef.ofTypeS p.PropertyType
                                                return (p.Name, t)
                                            }
                                        )

                                    return { uname = c.Name; ufields = fields }
                                }
                            )
                            
                        let! ref = ExternalTypeInfo.ofTypeS t

                        
                        let def = { dinfo = ref; ddef = DUnion(cases) }
                        do! State.modify (TranslationState.defineType t def)
                        return Some def

                    else   
                        let isReflectable = 
                            t.GetConstructors(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.CreateInstance)
                            |> Array.exists (Expr.TryGetReflectedDefinition >> Option.isSome)

                        if isReflectable then
                            let! fields = 
                                t.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                                |> Array.toList
                                |> List.mapS (fun p ->
                                    state {
                                        let! t = TypeRef.ofTypeS p.FieldType
                                        return (p.Name, t)
                                    }
                                )
                            let! ref = ExternalTypeInfo.ofTypeS t
                            let def = { dinfo = ref; ddef = DObject fields }
                            do! State.modify (TranslationState.defineType t def)
                            return Some def
                        else

                            return None
                }
                
        let ofTypeS (t : Type) =
            state {
                match! tryOfTypeS t with
                    | Some def -> return def
                    | None -> return failwithf "[FShade] cannot translate type %A" t
            }

    module FunctionDef =
        open Microsoft.FSharp.Quotations.DerivedPatterns

        let rec private constructorBody (t : Type) (e : Expr) : Expr =
            match e with
                | Sequential(Sequential(a,b), c) ->
                    constructorBody t (Expr.Sequential(a, Expr.Sequential(b,c)))

                | Lambdas(vs, Sequential(NewObject(_,_), b)) ->
                    let self = Var("self", t, true)
                    let body = 
                        Expr.Let(self, Expr.DefaultValue(t),
                            Expr.Sequential(
                                b.Substitute(fun v -> if v.Name = "this" then Some (Expr.Var self) else None),
                                Expr.Var self
                            )
                        )

                    let rec bindings (v : list<Var>) (e : list<Expr>) (b : Expr) =
                        match v, e with
                            | [], [] -> b
                            | v :: vs, e :: es -> Expr.Let(v,e,bindings vs es b)
                            | _ -> failwith "asdasdasd"

                    let rec wrap (vs : list<list<Var>>) (e : Expr) =
                        match vs with
                            | [] -> e
                            | h :: t ->
                                match h with 
                                    | [h] -> Expr.Lambda(h, wrap t e)
                                    | hs -> 
                                        let typ = FSharpType.MakeTupleType (hs |> List.map (fun h -> h.Type) |> List.toArray)
                                        let tup = Var("tup", typ)
                                        let get = hs |> List.mapi (fun i v -> Expr.TupleGet(Expr.Var tup, i))
                                        Expr.Lambda(tup, bindings hs get (wrap t e))

                    wrap vs body

                | _ ->
                    e

        let rec private memberBody (t : Type) (e : Expr) =
            match e with
                | Lambda(v,Lambda(u, b)) when u.Type = typeof<unit> ->
                    memberBody t (Expr.Lambda(v, b))
                | _ ->
                    printfn "%A" e
                    e
        let private getParameters (e : Expr) =
            match e with
                | Lambdas(vs, _) ->
                    vs
                | _ ->
                    []

        let tryOfMethodS (m : MethodBase) =
            state {
                let! s = State.get
                match HMap.tryFind m s.funDefs with
                    | Some d ->
                        return Some d
                    | None ->
                        match Expr.TryGetReflectedDefinition m with
                            | Some expr ->
                                let expr = 
                                    if m.IsConstructor then constructorBody m.DeclaringType expr
                                    elif not m.IsStatic then memberBody m.DeclaringType expr
                                    else expr

                                let! e = TExpr.ofExpr expr
                                
                                let! info = ExternalFunctionInfo.tryOfMethodS false m
                                let info = Option.get info
                                let! pars = 
                                    getParameters expr |> List.mapS (fun v ->
                                        v |> List.mapS (fun v ->
                                            state {
                                                let! t = TypeRef.ofTypeS v.Type
                                                return (v.Name, t)
                                            }
                                        )
                                    )

                                let info =
                                    {
                                        info with
                                            fdeclaring = ExternalTypeInfo.Top
                                            fparameters = pars
                                    }

                                let res =
                                    {
                                        finfo = info
                                        fexpr = e
                                    }
                                
                                do! State.modify (fun s ->
                                        let update (o : Option<Set<ExternalFunctionInfo>>) =
                                            match o with
                                                | Some s ->
                                                    let s = Set.remove info s
                                                    if Set.isEmpty s then None
                                                    else Some s
                                                | None ->
                                                    None
                                        { s with
                                            funImps = MapExt.alter info.fdeclaring.eassembly update s.funImps
                                            funRefs = HMap.add m info s.funRefs
                                            funDefs = HMap.add m res s.funDefs
                                        }
                                    )

                                return Some res

                            | None ->
                                return None
            }

    module FunctionRef =
        let ofMethodS (m : MethodBase) =
            state {
                match! FunctionDef.tryOfMethodS m with
                    | Some d -> return d.finfo
                    | None -> return! ExternalFunctionInfo.ofMethodS m
            }

    module TypeRef =
        let ofTypeS (t : Type) =
            state {
                let! s = State.get
                match HMap.tryFind t s.typeRefs with
                | Some t ->
                    return t
                | None -> 
                    match TypeRef.tryOfSimpleType t with
                    | Some simple ->
                        return simple
                    | None ->
                        if FSharpType.IsTuple(t) then
                            let! elements = 
                                FSharpType.GetTupleElements t
                                |> Array.toList
                                |> List.mapS ofTypeS
                            return RTuple elements

                        elif FSharpType.IsFunction(t) then
                            let a,b = FSharpType.GetFunctionElements(t)
                            let! a = ofTypeS a
                            let! b = ofTypeS b
                            return RFunction(a,b)

                        else
                            let! def = TypeDef.tryOfTypeS t
                            match def with
                            | Some def -> 
                                return RImported(def.dinfo, Some def.ddef)
                            | None ->
                                let! e = ExternalTypeInfo.ofTypeS t
                                return RImported(e, None)
                            
            }

    module ExternalFunctionInfo =

        let private getParameterBlocksS (m : MethodBase) =
            state {
                let! pars = 
                    m.GetParameters()
                    |> Array.toList
                    |> List.mapS (fun p ->
                        state {
                            let! t = TypeRef.ofTypeS p.ParameterType
                            return p.Name, t
                        }
                    )

                let att = 
                    let a = m.GetCustomAttribute<CompilationArgumentCountsAttribute>()
                    if isNull (a :> Attribute) then None
                    else Some(Seq.toList a.Counts)

                match att with
                    | None ->
                        match pars with
                            | [] -> return []
                            | _ -> return [pars]
                    | Some att ->
                        let rec take (n : int) (l : list<'a>) =
                            if n <= 0 then 
                                [], l
                            else
                                match l with
                                | h :: t ->
                                    let a,b = take (n - 1) t
                                    h::a, b
                                | _ ->
                                    failwith "index out of range"

                        return [
                            let mutable rem = pars
                            for c in att do
                                let (a,b) = take c rem
                                yield a
                                rem <- b
                                
                            if not (List.isEmpty rem) then 
                                failwithf "[FShade] bad argument counts for method %A" m
                        ]
            }

        let tryOfMethodS (import : bool) (mb : MethodBase) =
            state {
                let! s = State.get
                match HMap.tryFind mb s.funRefs with
                    | Some r ->
                        return Some r
                    | None -> 
                        let! d = ExternalTypeInfo.ofTypeS mb.DeclaringType
                        let! self = TypeRef.ofTypeS mb.DeclaringType
                        let! gen =
                            if mb.IsGenericMethod then mb.GetGenericArguments() |> Array.toList |> List.mapS TypeRef.ofTypeS
                            else State.value []

                        let! pars = getParameterBlocksS mb
                        
                        match mb with
                        | :? MethodInfo as m ->
                            let! ret = TypeRef.ofTypeS m.ReturnType
                            let res = 
                                {
                                    fdeclaring  = d
                                    fname       = m.Name
                                    fgeneric    = gen
                                    fparameters = pars
                                    freturn     = ret
                                }
                            if import then
                                do! State.modify (TranslationState.importFunction res)
                                do! State.modify (fun s -> { s with funRefs = HMap.add mb res s.funRefs })

                            return Some res
      
                        | :? ConstructorInfo as c ->
                            let! ret = TypeRef.ofTypeS mb.DeclaringType
                            let res = 
                                {
                                    fdeclaring  = d
                                    fname       = sprintf "%s_ctor" d.ename
                                    fgeneric    = gen
                                    fparameters = pars
                                    freturn     = ret
                                }
                                
                            if import then
                                do! State.modify (TranslationState.importFunction res)
                                do! State.modify (fun s -> { s with funRefs = HMap.add mb res s.funRefs })
                            return Some res
                            

                        | _ ->
                            return None
            }

        let ofMethodS (m : MethodBase) =
            state {
                match! tryOfMethodS true m with
                    | Some m -> return m
                    | None -> return failwithf "[FShade] cannot import function %A" m
            }

    module TVar =
        let ofVarS (v : Var) =
            state {
                let! s = State.get
                match MapExt.tryFind v s.variables with
                    | Some v ->
                        return v
                    | None ->
                        let! typ = TypeRef.ofTypeS v.Type
                        let r = TVar(v.Name, typ, v.IsMutable)
                        do! State.modify (TranslationState.declare v r)
                        return r
            }

  
    module TExpr =
        let rec ofExpr (e : Expr) =
            state {
                match e with
                    | Value(v,t) -> 
                        let! t = TypeRef.ofTypeS t
                        return TExpr.Value(v, t)

                    | Var v ->
                        let! v = TVar.ofVarS v
                        return TExpr.Var v
                
                    | Call(Some t, mi, args) ->
                        let! f = FunctionRef.ofMethodS mi
                        let! args = List.mapS ofExpr (t :: args)
                        return TExpr.Call(f, args)
                        

                    | Call(None, mi, args) ->
                        let! f = FunctionRef.ofMethodS mi
                        let! args = List.mapS ofExpr args
                        return TExpr.Call(f, args)

                    | Lambda(v, b) ->
                        let! v = TVar.ofVarS v
                        let! b = ofExpr b
                        return TExpr.Lambda(v, b)

                    | NewRecord(t, values) ->
                        let! t = TypeRef.ofTypeS t
                        let! values = values |> List.mapS ofExpr
                        return TExpr.NewRecord(t, values)

                    | NewObject(ctor, values) ->
                        let! c = FunctionRef.ofMethodS ctor
                        let! values = values |> List.mapS ofExpr
                        return TExpr.Call(c, values)

                    | Sequential(l,r) ->
                        let! l = ofExpr l
                        let! r = ofExpr r
                        return TExpr.Sequential(l,r)
                        
                    | FieldSet(Some t, n, v) ->
                        let! t = ofExpr t
                        let! v = ofExpr v
                        return TExpr.FieldSet(t, n.Name, v)
                        
                    | FieldGet(Some t, n) ->
                        let! t = ofExpr t
                        let! typ = TypeRef.ofTypeS e.Type
                        return TExpr.FieldGet(t, n.Name, typ)

                    | Let(v,e,b) ->
                        let! v = TVar.ofVarS v
                        let! e = ofExpr e
                        let! b = ofExpr b
                        return TExpr.Let(v,e,b)

                    | DefaultValue t ->
                        let! t = TypeRef.ofTypeS t
                        return TExpr.Default t

                    | PropertyGet(Some e, prop, []) when FSharpType.IsRecord e.Type ->
                        let! e = ofExpr e
                        let! t = TypeRef.ofTypeS prop.PropertyType
                        return TExpr.FieldGet(e, prop.Name, t)
                        
                    | PropertyGet(Some e, prop, []) ->
                        return! ofExpr (Expr.Call(e, prop.GetMethod, []))

                    | e ->
                        return failwithf "implement me: %A" e
            } 

    let translate (e : Expr) =
        let mutable s = TranslationState.Empty
        let ex = TExpr.ofExpr(e).Run(&s)

        let m = TranslationState.toModule s


        let def = FunctionDef.ofExpr "main" ex
        { m with mfunctions = Map.add def.finfo def m.mfunctions }
        
type Rec =
    {
        a : int
        b : int
    }

[<ReflectedDefinition>]
type A(a : int) =
    member x.A = a
    member x.Bla() = a * 2
[<ReflectedDefinition>]
let test (a : int) (b : int) = a + b

let ex = <@ fun (a : int) -> A(a).Bla() @>
