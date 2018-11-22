#load "load.fsx"

open Aardvark.Base
open Aardvark.Base.Monads.State


[<AutoOpen>]
module rec Stuff =
    
    type TypeDef =
        | DRecord of name : string * fields : list<string * TypeRef> 

    module TypeDef =
        let toString (d : TypeDef) =
            match d with
            | DRecord(name, fields) ->
                fields
                |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                |> String.concat "; "
                |> sprintf "type %s = { %s }" name

    type TypeRef =
        | RInt of signed : bool * bits : int
        | RFloat of bits : int
        | RChar
        | RString

        | RType of name : string
        | RTuple of elements : list<TypeRef>
        | RFunction of TypeRef * TypeRef

    module TypeRef =
        let rec toString (t : TypeRef) =
            match t with
            | RInt(false, 8) -> "byte"
            | RInt(true, 32) -> "int"
            | RFloat(64) -> "float"


            | RInt(true, b) -> sprintf "int%d" b
            | RInt(false, b) -> sprintf "uint%d" b
            | RFloat b -> sprintf "float%d" b
            | RChar -> "char"
            | RString -> "string"

            | RTuple [e] -> toString e
            | RTuple elements -> elements |> List.map toString |> String.concat " * " |> sprintf "(%s)"
            | RType name -> name
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


    type FunctionRef =
        {
            fname           : string
            fparameters     : list<list<string * TypeRef>>
            freturn         : TypeRef
        }

        member x.ftype =
            let types = x.fparameters |> List.map (List.map snd >> RTuple)
            List.foldBack (fun e s -> RFunction(e,s)) types x.freturn 

    module FunctionRef =
        let toString (s : FunctionRef) : string =
            let args =
                s.fparameters |> List.map (fun block ->
                    block
                    |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                    |> String.concat ", "
                    |> sprintf "(%s)"
                )
                |> String.concat " "
            sprintf "%s %s : %s" s.fname args (TypeRef.toString s.freturn)

    type Literal =
        | LString of value : string
        | LChar of value : char
        | LInt of signed : bool * bits : int * value : uint64
        | LFloat of bits : int * value : float

        member x.ltype =
            match x with
                | LString _ -> RString
                | LChar _ -> RChar
                | LInt(s,b,_) -> RInt(s,b)
                | LFloat(b,_) -> RFloat(b)

    module Literal =
        let toString (l : Literal) =
            match l with
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
    type Var(name : string, typ : TypeRef, isMutable : bool, id : int) =
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


    type ExprInfo =
        | EValue of value : Literal
        | EVar of Var
        | ECall of f : FunctionRef
        | ELambda of Var
        
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
        static member Call(f : FunctionRef, args : list<Expr>) = 
            let argTypes = args |> List.map (fun e -> e.Type)
            match TypeRef.tryApply argTypes f.ftype with
                | Some ret ->
                    Expr(ECall f, ret, args)
                | None ->
                    failwithf 
                        "[FShade] inconsistent argument types for %s: [%s]" 
                        (FunctionRef.toString f) 
                        (argTypes |> List.map TypeRef.toString |> String.concat "; ")

        static member Lambda(v : Var, e : Expr) =
            let t = RFunction(v.Type, e.Type)
            Expr(ELambda v, t, [e])

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

                    sprintf "%s(%s)" f.fname (String.concat ", " args)
                | Lambdas(vs, b) ->
                    let vs = vs |> List.map (fun v -> sprintf "(%s : %s)" v.Name (TypeRef.toString v.Type)) |> String.concat " "
                    sprintf "fun %s -> %s" vs (toString b)
                
                | _ ->
                    failwith "unreachable"

    type FunctionDef =
        {
            fsignature      : FunctionRef
            fexpr           : Expr
        }
    module FunctionDef =
        open Patterns
        
        let toString (d : FunctionDef) =
            match d.fexpr with
                | Lambdas(vs, b) ->
                    let args =
                        d.fsignature.fparameters |> List.map (fun block ->
                            block
                            |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                            |> String.concat ", "
                            |> sprintf "(%s)"
                        )
                        |> String.concat " "
                    sprintf "let %s (%s) : %s = %s" d.fsignature.fname args (TypeRef.toString b.Type) (Expr.toString b)

                | b ->
                    sprintf "let %s : %s = %s" d.fsignature.fname (TypeRef.toString b.Type) (Expr.toString b)
                
        
    type Module =
        {
            mname       : string
            mtypes      : Map<string, TypeDef>
            mfunctions  : Map<FunctionRef, FunctionDef>
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

module AdapterStuff =
    open System
    open System.Reflection
    open Stuff
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    type private TExpr = Stuff.Expr
    type private TVar = Stuff.Var

    type TranslationState =
        {
            types       : hmap<Type, TypeRef>
            functions   : hmap<MethodInfo, FunctionRef>
            variables   : MapExt<Var, TVar>
            currentId   : int
        }

        static member Empty = { types = HMap.empty; functions = HMap.empty; variables = MapExt.empty; currentId = 0 }

    module private Translate =

        let getType (t : System.Type) =
            State.custom (fun s ->
                match HMap.tryFind t s.types with
                    | Some t ->
                        s, t
                    | None ->
                        let r = TypeRef.tryOfSimpleType t |> Option.get
                        let s = { s with types = HMap.add t r s.types }
                        s, r
            )

        let getVar (v : Var) =
            State.custom (fun s ->
                match MapExt.tryFind v s.variables with
                    | Some v ->
                        s, v
                    | None ->
                        let mutable s = s
                        let typ = getType(v.Type).Run(&s)
                        let r = TVar(v.Name, typ, v.IsMutable, s.currentId)
                        let s = { s with variables = MapExt.add v r s.variables; currentId = s.currentId + 1 }
                        s, r
            )

        let getFunction (mi : MethodInfo) =
            State.custom (fun s ->
                match HMap.tryFind mi s.functions with
                    | Some v ->
                        s, v
                    | None ->
                        let mutable s = s
                        let args = 
                            mi.GetParameters() 
                            |> Array.map (fun p -> [p.Name, getType(p.ParameterType).Run(&s)]) 
                            |> Array.toList

                        let ret = getType(mi.ReturnType).Run(&s)

                        let r = 
                            {
                                fname = mi.Name
                                fparameters = args
                                freturn = ret
                            }
                        let s = { s with functions = HMap.add mi r s.functions }
                        s, r

            )
            

    let rec translateS (e : Expr) =
        state {
            match e with
                | Value(v,t) -> 
                    let! t = Translate.getType t
                    return TExpr.Value(v, t)

                | Var v ->
                    let! v = Translate.getVar v
                    return TExpr.Var v
                
                | Call(None, mi, args) ->
                    let! f = Translate.getFunction mi
                    let! args = List.mapS translateS args
                    return TExpr.Call(f, args)

                | Lambda(v, b) ->
                    let! v = Translate.getVar v
                    let! b = translateS b
                    return TExpr.Lambda(v, b)

                | _ ->
                    return failwith "implement me"
        } 

    let translate (e : Expr) =
        let mutable s = TranslationState.Empty
        translateS(e).Run(&s)
        
module rec Expressions =
    
    type Module =
        {
            mtypes      : Map<string, TypeDef>
            mfunctions  : Map<string, FunctionDef>

        }

    type FunctionSignature =
        {
            fname           : string
            fparameters     : list<list<string * TType>>
            freturn         : TType
        }

    type FunctionDef =
        {
            fsignature      : FunctionSignature
            fexpr           : TExpr
        }

    type TypeDef =
        | DRecord of name : string * fields : list<string * TType> 


    type TType =
        | TChar
        | TString
        | TInt of signed : bool * width : int
        | TFloat of width : int
        | TArray of dim : int * content : TType
    
        | TVector of dim : int * content : TType
        | TMatrix of rows : int * cols : int * content : TType

        | TTuple of list<TType>
        | TRecord of list<string * TType>
        | TUnion of list<string * list<string * TType>>
        | TFunction of TType * TType

    module TType =
        open System

        let rec toString (t : TType) =
            match t with
        
                | TInt(false, 8) -> "byte"
                | TInt(true, 8) -> "sbyte"
                | TInt(true, 32) -> "int"
                | TFloat(64) -> "float"

                | TChar -> "char"
                | TString -> "string"
                | TInt(true,w) -> sprintf "int%d" w
                | TInt(false,w) -> sprintf "uint%d" w
                | TFloat w -> sprintf "float%d" w
                | TArray(dim, bt) -> 
                    let str = System.String(',', dim - 1)
                    sprintf "%s[%s]" (toString bt) str
                | TVector(dim, c) ->
                    sprintf "Vec%d%s" dim (toString c)
                | TMatrix(r,c, e) ->
                    sprintf "Mat%d%d%s" r c (toString e)
                | TTuple args ->
                    args |> List.map toString |> String.concat " * " |> sprintf "(%s)"
                | TRecord fields ->
                    fields |> List.map (fun (name, typ) -> sprintf "%s : %s" name (toString typ)) |> String.concat "; " |> sprintf "{ %s }"

                | TUnion cases ->
                    cases
                    |> List.map (fun (name, fields) -> 
                        fields
                        |> List.map (fun (name, typ) -> sprintf "%s : %s" name (toString typ))
                        |> String.concat " * "
                        |> sprintf "%s of %s" name
                    )
                    |> String.concat " | "
                | TFunction(a,b) ->
                    sprintf "%s -> %s" (toString a) (toString b)

        let rec apply (targs : list<TType>) (fType : TType) =
            match targs with
                | [] -> fType
                | h :: t ->
                    match fType with
                        | TFunction(a,b) ->
                            if a <> h then failwithf "[FShade] inconsistent function argument. is %A but should be %A" h a
                            apply t b
                        | _ ->
                            failwith "[FShade] not a function"

        let rec ofType (t : System.Type) =
            if t = typeof<int8> then TInt(true, 8)
            elif t = typeof<uint8> then TInt(false, 8)
            elif t = typeof<int16> then TInt(true, 16)
            elif t = typeof<uint16> then TInt(false, 16)
            elif t = typeof<int32> then TInt(true, 32)
            elif t = typeof<uint32> then TInt(false, 32)
            elif t = typeof<int64> then TInt(true, 64)
            elif t = typeof<uint64> then TInt(false, 64)
            elif t = typeof<float32> then TFloat(32)
            elif t = typeof<float> then TFloat(64)
            else failwith "not a primitive type"

    type TLiteral =
        | LInt of uint64
        | LFloat of float
        | LChar of char
        | LString of string
    
    module TLiteral =
        let toObject (t : TType) (l : TLiteral) =
            match t, l with
                | TInt(true, 8),    LInt v      -> int8 v :> obj
                | TInt(false, 8),   LInt v      -> uint8 v :> obj
                | TInt(true, 16),   LInt v      -> int16 v :> obj
                | TInt(false, 16),  LInt v      -> uint16 v :> obj
                | TInt(true, 32),   LInt v      -> int32 v :> obj
                | TInt(false, 32),  LInt v      -> uint32 v :> obj
                | TInt(true, 64),   LInt v      -> int64 v :> obj
                | TInt(false, 64),  LInt v      -> uint64 v :> obj

                | TFloat(32),       LFloat v    -> float32 v :> obj
                | TFloat(64),       LFloat v    -> float v :> obj

                | TChar,            LChar c     -> c :> obj
                | TString,          LString str -> str :> obj

                | _ -> failwithf "[Expr] inconsistent type (%A) for literal %A" t l

        let ofObject (v : obj) =
            match v with
                | :? int8 as o -> LInt(uint64 o)
                | :? uint8 as o -> LInt(uint64 o)
                | :? int16 as o -> LInt(uint64 o)
                | :? uint16 as o -> LInt(uint64 o)
                | :? int32 as o -> LInt(uint64 o)
                | :? uint32 as o -> LInt(uint64 o)
                | :? int64 as o -> LInt(uint64 o)
                | :? uint64 as o -> LInt(uint64 o)
                | :? float32 as o -> LFloat(float o)
                | :? float as o -> LFloat(o)
                | :? char as o -> LChar(o)
                | :? string as o -> LString(o)
                | _ -> failwith "not a primitive value"

        let toString (l : TLiteral) =
            match l with
                | LInt v -> sprintf "%d" v
                | LFloat v -> sprintf "%f" v
                | LChar c -> sprintf "'%c'" c
                | LString str -> sprintf "\"%s\"" str

    type TVar(name : string, vtype : TType, isMutable : bool) =
        member x.Name = name
        member x.Type = vtype
        member x.IsMutable = isMutable
        new(name,typ) = TVar(name, typ, false)

    module TVar =
        let toString (v : TVar) = v.Name

    type TFunctionInfo =
        {
            fname : string
            ftype : TType
        }
    

    type TExprInfo =
        | ELiteral of TLiteral
        | EVar of TVar
        | EApplication of TFunctionInfo 
        | ELambda of TVar

    and TExpr(etype : TType, einfo : TExprInfo, echildren : list<TExpr>) = 
        member x.Type = etype
        member x.Info = einfo
        member x.Children = echildren

        override x.ToString() =
            match einfo with
                | ELiteral o -> TLiteral.toString o
                | EVar v -> TVar.toString v
                | EApplication f ->
                    echildren |> List.map string |> String.concat ", " |> sprintf "%s(%s)" f.fname
                | _ ->
                    match x with
                        | Patterns.Lambdas(vs, b) ->
                            let args = vs |> List.map (fun v -> sprintf "(%s : %s)" (TVar.toString v) (TType.toString v.Type)) |> String.concat " "
                            sprintf "fun %s -> %s" args (string b)
                        | _ ->
                            failwith ""




        static member Value(o : 'a) =
            let t = TType.ofType typeof<'a>
            let v = TLiteral.ofObject o
            TExpr.Value(t, v)

        static member Value(t : TType, l : TLiteral) = TExpr(t, ELiteral l, [])
        static member Var(v : TVar) = TExpr(v.Type, EVar v, [])
        static member Application(f : TFunctionInfo, args : list<TExpr>) =
            let argTypes = args |> List.map (fun e -> e.Type)
            let t = TType.apply argTypes f.ftype
            TExpr(t, EApplication f, args)

        static member Lambda(v : TVar, b : TExpr) =
            let t = TFunction(v.Type, b.Type)
            TExpr(t, ELambda v, [b])

    module Patterns =
    
        let (|Value|_|) (e : TExpr) =
            match e.Info with
            | ELiteral v -> Some (TLiteral.toObject e.Type v)
            | _ -> None
        
        let (|Var|_|) (e : TExpr) =
            match e.Info with
            | EVar v -> Some v
            | _ -> None

        let (|Application|_|) (e : TExpr) =
            match e.Info with
            | EApplication v -> Some(v, e.Children)
            | _ -> None
        
        let (|Lamdda|_|) (e : TExpr) =
            match e.Info with
            | ELambda v -> Some(v, List.head e.Children)
            | _ -> None

        let rec (|Lambdas|_|) (e : TExpr) =
            match e with
                | Lamdda(a, x) ->
                    match x with
                        | Lambdas (b,c) ->
                            Some (a::b, c)
                        | _ ->
                            Some ([a], x)
                | _ ->
                    None

    module Adapter =
        open Microsoft.FSharp.Quotations
        open Microsoft.FSharp.Quotations.Patterns

        let rec toTExpr (e : Expr) =
            match e with
                | Value(v,t) -> TExpr.Value(TType.ofType t, TLiteral.ofObject v)
                | Var v -> TExpr.Var(TVar(v.Name, TType.ofType v.Type, v.IsMutable))
                | Call(None, mi, args) ->
                    let r = TType.ofType mi.ReturnType
                    let targs = mi.GetParameters() |> Array.map (fun p -> p.ParameterType |> TType.ofType)
                    let ftype = Array.foldBack (fun e f -> TFunction(e, f)) targs r

                    let info =
                        {
                            fname = mi.Name
                            ftype = ftype
                        }
                    TExpr.Application(info, List.map toTExpr args)

                | Lambda(v, b) ->
                    let v = TVar(v.Name, TType.ofType v.Type, v.IsMutable)
                    TExpr.Lambda(v, toTExpr b)

                | _ ->
                    failwith ""


    let test = Adapter.toTExpr <@ 1 + 2 @>