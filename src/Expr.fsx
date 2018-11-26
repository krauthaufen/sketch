#load "load.fsx"

open System
open System.Reflection
open System.Threading
open Aardvark.Base
open Aardvark.Base.Monads.State
open Microsoft.FSharp.Reflection

module rec Ast =
    
    [<Struct; CustomEquality; CustomComparison; StructuredFormatDisplay("{AsString}")>]
    type TypeId(id : int) =
        member x.Id = id

        member private x.AsString = x.ToString()

        interface IComparable with
            member x.CompareTo o =
                match o with
                | :? TypeId as o -> compare id o.Id
                | _ -> failwith "cannot compare"
                
        interface IComparable<TypeId> with
            member x.CompareTo o = compare id o.Id

        override x.ToString() = sprintf "T%d" id
        override x.GetHashCode() = id
        override x.Equals o =
            match o with
            | :? TypeId as o -> id = o.Id
            | _ -> false


    type TypeInfo =
        {
            eassembly   : string
            enamespace  : list<string>
            ename       : string
            eismodule   : bool
            egeneric    : list<TypeId>
            eattributes : list<AttributeValue>
        }

        static member Top  =
            {
                eassembly = ""
                enamespace = []
                ename = "Entry"
                eismodule = true
                egeneric = []
                eattributes = []
            }

        member x.efullname = 
            x.enamespace @ [x.ename] |> String.concat "."

    module TypeInfo =
        let toString (i : TypeInfo) = i.efullname


    type UnionCase =
        {
            uname       : string
            ufields     : list<string * TypeId>
        }

    type TypeDef =
        | DUnit
        | DInt of signed : bool * bits : int
        | DFloat of bits : int
        | DBool
        | DChar
        | DString
        | DType

        | DEnum of signed : bool * bits : int * values : list<string * uint64>
        | DArray of elementType : TypeId
        | DTuple of elements : list<TypeId>
        | DFunction of TypeId * TypeId
        | DRecord of info : TypeInfo * fields : list<string * TypeId> 
        | DUnion of info : TypeInfo * cases : list<UnionCase>
        | DObject of info : TypeInfo * fields : list<string * TypeId>

    module TypeDef =

        let rec name (ctx : Context) (d : TypeDef) =
            match d with

                | DInt(true, 32) -> "int"
                | DInt(false, 8) -> "byte"
                | DInt(true, 8) -> "sbyte"
                | DFloat(64) -> "float"

                | DArray(e) -> sprintf "array<%s>" (resolve ctx e)
                | DEnum(s,b,_) -> sprintf "enum<%s>" (name ctx (DInt(s,b)))
                | DUnit -> "unit"
                | DInt(true, b) -> sprintf "int%d" b
                | DInt(false, b) -> sprintf "uint%d" b
                | DFloat b -> sprintf "float%d" b
                | DBool -> "bool"
                | DChar -> "char"
                | DString -> "string"
                | DType -> "System.Type"
                | DTuple es -> es |> List.map (resolve ctx) |> String.concat " * "
                | DFunction(a,b) -> sprintf "%s -> %s" (resolve ctx a) (resolve ctx b)
                | DRecord(i,_) -> TypeInfo.toString i
                | DUnion(i,_) -> TypeInfo.toString i
                | DObject(i,_) -> TypeInfo.toString i

        and private resolve (ctx : Context) (id : TypeId) : string =
            match ctx.TryGetTypeDefinition id with
                | Some def -> name ctx def
                | None -> ctx.GetTypeInfo id |> TypeInfo.toString
   
   
    type Literal =
        | LInt of signed : bool * bits : int * value : uint64
        | LFloat of bits : int * value : float
        | LBool of value : bool
        | LChar of value : char
        | LString of value : string
        | LUnit
        | LNull of typ : TypeId
        | LArray of elementType : TypeId * values : list<Literal>
        | LType of TypeId
        | LEnum of signed : bool * bits : int * cases : list<string * uint64> * value : uint64
        member x.GetType(ctx : Context) =
            match x with
                | LInt(s,b,_) -> DInt(s,b)
                | LFloat(b,_) -> DFloat(b)
                | LBool _ -> DBool
                | LChar _ -> DChar
                | LString _ -> DString
                | LUnit -> DUnit
                | LNull t -> ctx.GetTypeDefinition t
                | LArray(t,_) -> DArray t
                | LType _ -> DType
                | LEnum(s,b,c,_) -> DEnum(s,b,c)

    module Literal =
        let ofTypedValue (ctx : Context) (tid : TypeId) (value : obj) =
            let t = ctx.GetTypeDefinition tid
            if isNull value then
                match t with
                    | DUnit -> LUnit
                    | _ -> LNull tid
            else
                match t with
                    | DEnum(s,b,vs) -> LEnum(s,b,vs, System.Convert.ToUInt64 value)
                    | DInt(s,b) -> LInt(s, b, System.Convert.ToUInt64 value)
                    | DFloat(b) -> LFloat(b, System.Convert.ToDouble value)
                    | DBool -> LBool(unbox value)
                    | DChar -> LChar(unbox value)
                    | DString -> LString (unbox value)
                    | DUnit -> LUnit
                    | DArray t ->
                        let arr = value |> unbox<System.Collections.IEnumerable>
                        let values =
                            [
                                let e = arr.GetEnumerator()
                                while e.MoveNext() do
                                    yield ofTypedValue ctx t e.Current
                            ]
                        LArray(t, values)
                    | DType -> LType(unbox value)

                    | DFunction _ | DObject _ | DRecord _ | DUnion _ | DTuple _->
                        failwithf "%A cannot be a literal" t
        
    type AttributeValue =
        {
            atype : TypeId
            aargs : list<Literal>
        }



           
    type Context =
        {
            typeIds     : Set<TypeId>
            typeInfos   : Map<TypeId, TypeInfo>
            typeDefs    : Map<TypeId, TypeDef>
        }
        static member Empty = { typeIds = Set.empty; typeInfos = Map.empty; typeDefs = Map.empty }

        member x.HasTypeInfo(id : TypeId) = Map.containsKey id x.typeInfos
        member x.TryGetTypeInfo(id : TypeId) = Map.tryFind id x.typeInfos
        member x.TryGetTypeDefinition(id : TypeId) = Map.tryFind id x.typeDefs
        
        member x.GetTypeInfo(id : TypeId) = Map.find id x.typeInfos
        member x.GetTypeDefinition(id : TypeId) = Map.find id x.typeDefs

    module Context =
        let inline tryGetTypeInfo (id : TypeId) (c : Context) = c.TryGetTypeInfo id
        let inline tryGetTypeDefinition (id : TypeId) (c : Context) = c.TryGetTypeDefinition id
        let inline getTypeInfo (id : TypeId) (c : Context) = c.GetTypeInfo id
        let inline getTypeDefinition (id : TypeId) (c : Context) = c.GetTypeDefinition id

module rec Translate =
    open Ast

    type TranslationState =
        {
            currentId   : int
            typeIds     : hmap<Type, TypeId>
            context     : Context
        }

        static member Empty = { currentId = 0; typeIds = HMap.empty; context = Context.Empty }

    module TranslationState =
        let modifyContext (mapping : Context -> Context) =
            State.modify (fun s -> { s with context = mapping s.context })

        let newTypeId = 
            State.custom (fun s ->
                let id = s.currentId
                { s with currentId = id + 1}, TypeId(id)
            )

        let getTypeId (t : Type) =
            state {
                let! s = State.get
                match HMap.tryFind t s.typeIds with
                    | Some id -> 
                        return id
                    | None ->
                        let! id = newTypeId
                        do! State.modify (fun s -> { s with context = { s.context with typeIds = Set.add id s.context.typeIds }; typeIds = HMap.add t id s.typeIds })
                        return id
            }
            
        let setTypeInfo (id : TypeId) (info : TypeInfo) =
            modifyContext (fun ctx ->
                { ctx with typeInfos = Map.add id info ctx.typeInfos }
            )

        let setTypeDef (id : TypeId) (def : TypeDef) =
            modifyContext (fun ctx ->
                { ctx with typeDefs = Map.add id def ctx.typeDefs }
            )

        let tryGetTypeInfo (id : TypeId) =
            State.get |> State.map (fun s ->
                s.context.TryGetTypeInfo id
            )

        let tryGetTypeDef (id : TypeId) =
            State.get |> State.map (fun s ->
                s.context.TryGetTypeDefinition id
            )

    module TypeInfo =

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

        let ofTypeS (t : Type) =
            state {
                let! tid = TranslationState.getTypeId t
                match! TranslationState.tryGetTypeInfo tid with
                | Some info ->
                    return info
                | None ->
                    let ass = t.Assembly.GetName().Name

                    let! att = t.GetCustomAttributesData() |> Seq.toList |> List.mapS (AttributeValue.ofAttributeS)

                    match t with
                    | Generic(ns, name, cnt) ->
                        let! targs = t.GetGenericArguments() |> Array.toList |> List.mapS TranslationState.getTypeId
                        let info =  
                            {
                                eassembly = ass
                                enamespace = ns
                                ename = name
                                egeneric = targs
                                eismodule = false
                                eattributes = att
                            }

                        do! TranslationState.setTypeInfo tid info
                        return info

                    | NonGeneric(ns, name) ->
                        let info = 
                            {
                                eassembly = ass
                                enamespace = ns
                                ename = name
                                egeneric = []
                                eismodule = FSharpType.IsModule t
                                eattributes = att
                            }
                        do! TranslationState.setTypeInfo tid info
                        return info
            }

    module AttributeValue =
        let ofAttributeS (a : System.Reflection.CustomAttributeData) =
            state {
                let! t = TranslationState.getTypeId a.AttributeType
                
                let! args = 
                    a.ConstructorArguments |> Seq.toList |> List.mapS (fun a ->
                        state {
                            let! tid = TranslationState.getTypeId a.ArgumentType
                            let! t = TypeDef.ofTypeS a.ArgumentType
                            let! s = State.get
                            let v = Literal.ofTypedValue s.context tid a.Value
                            return v
                        }
                    )

                return { atype = t; aargs = args }

            }

    module TypeDef =
        let tryOfSimpleType =
            LookupTable.lookupTable' [
                typeof<char>, DChar
                typeof<string>, DString

                typeof<int8>, DInt(true, 8)
                typeof<uint8>, DInt(false, 8)
                typeof<int16>, DInt(true, 16)
                typeof<uint16>, DInt(false, 16)
                typeof<int32>, DInt(true, 32)
                typeof<uint32>, DInt(false, 32)
                typeof<int64>, DInt(true, 64)
                typeof<uint64>, DInt(false, 64)

                typeof<float16>, DFloat(16)
                typeof<float32>, DFloat(32)
                typeof<float>, DFloat(64)
                
                typeof<unit>, DUnit
                typeof<bool>, DBool
                typeof<System.Void>, DUnit
                typeof<System.Type>, DType
            ]
            
        let tryOfTypeS (t : Type) =
            state {
                let! tid = TranslationState.getTypeId t
                match! TranslationState.tryGetTypeDef tid with
                | Some def ->
                    return Some def
                | None -> 
                    printfn "translate %A" t
                    match tryOfSimpleType t with
                    |  Some def ->
                        let! info = TypeInfo.ofTypeS t 
                        
                        do! TranslationState.setTypeDef tid def
                        return Some def
                    | None ->
                        if t.IsArray then
                            let! inner = TranslationState.getTypeId (t.GetElementType())
                            let def = DArray inner
                            do! TranslationState.setTypeDef tid def
                            return Some def
                        elif t.IsEnum then
                            let arr = System.Enum.GetValues(t)
                            let def = 
                        else 
                            let! info = TypeInfo.ofTypeS t 
                            if FSharpType.IsRecord(t, true) then
                                let! fields = 
                                    FSharpType.GetRecordFields(t, true)
                                    |> Array.toList
                                    |> List.mapS (fun p ->
                                        state {
                                            let! t = TranslationState.getTypeId p.PropertyType
                                            return (p.Name, t)
                                        }
                                    )
                            
                                let def = DRecord(info, fields)
                                do! TranslationState.setTypeDef tid def
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
                                                        let! t = TranslationState.getTypeId p.PropertyType
                                                        return (p.Name, t)
                                                    }
                                                )

                                            return { uname = c.Name; ufields = fields }
                                        }
                                    )
                            
                        
                                let def = DUnion(info, cases)
                                do! TranslationState.setTypeDef tid def
                                return Some def

                            else   
                                let! fields = 
                                    t.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                                    |> Array.toList
                                    |> List.mapS (fun p ->
                                        state {
                                            let! t = TranslationState.getTypeId p.FieldType
                                            return (p.Name, t)
                                        }
                                    )

                                let def = DObject(info, fields)
                                do! TranslationState.setTypeDef tid def
                                return Some def
                                
            }
            
        let ofTypeS (t : Type) : State<TranslationState, TypeDef> =
            state {
                match! tryOfTypeS t with
                    | Some d -> return d
                    | None -> return failwithf "no type definition for %A" t
            }

    let rec loadAllTypes() =
        state {
            let! s = State.get
            let missing = HMap.filter (fun t tid -> not (s.context.HasTypeInfo tid)) s.typeIds
            if not (HMap.isEmpty missing) then
                //printfn "missing: %A" (missing)
                for (t,tid) in missing do
                    let! def = TypeDef.tryOfTypeS t
                    match def with
                        | None ->
                            let! info = TypeInfo.ofTypeS t
                            ()
                        | _ ->
                            ()
                do! loadAllTypes()
        }

    let translateTypesS (ts : list<Type>) =
        state {
            let! defs = ts |> List.mapS TypeDef.ofTypeS
            do! loadAllTypes()
            return defs
        }

    let translateTypes (ts : list<Type>) =
        let mutable state = TranslationState.Empty
        let res = translateTypesS(ts).Run(&state)
        state.context, res

[<AutoOpen>]
module rec Stuff =

    type TypeInfo =
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
            dinfo   : TypeInfo
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
        | RBool
        | RChar
        | RString
        | RType
        | RImported of info : TypeInfo * def : Option<TypeDefInfo>
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
            | RBool -> "bool"
            | RChar -> "char"
            | RString -> "string"
            | RType -> "System.Type"

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
                typeof<bool>, RBool
                typeof<System.Void>, RUnit
                typeof<System.Type>, RType
            ]

        let tryApply (args : list<TypeRef>) (fType : TypeRef) =
            match args with
                | [] -> 
                    Some fType
                | a :: b ->
                    
                    match fType with
                    | RFunction(h,t)
                    | RFunction(RTuple [h], t) when a = h -> tryApply b t

                    | RFunction(RTuple (h :: hs), t) when a = h ->
                        match hs with
                            | [] -> tryApply b t
                            | _ -> tryApply b (RFunction(RTuple hs, t))

                    | _ -> None


    type FunctionInfo =
        {
            fdeclaring  : TypeInfo
            fname       : string
            fstatic     : bool
            fgeneric    : list<TypeRef>
            fparameters : list<list<string * TypeRef>>
            freturn     : TypeRef
        }

        member x.ffullname = 
            x.fdeclaring.efullname + "." + x.fname

        static member Entry(name : string, typ : TypeRef) =
            {
                fdeclaring = TypeInfo.Top
                fname = name
                fgeneric = []
                fparameters = []
                freturn = typ
                fstatic = true
            }

        member x.ftype =
            let tup = function [e] -> e | es -> RTuple es
            let types = x.fparameters |> List.map (List.map snd >> tup)
            List.foldBack (fun e s -> RFunction(e,s)) types x.freturn 
  
    module FunctionInfo =
        let toString (s : FunctionInfo) : string =
            let decl = s.fdeclaring.efullname
            let args =
                s.fparameters |> List.map (fun block ->
                    block
                    |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                    |> String.concat ", "
                    |> sprintf "(%s)"
                )
                |> String.concat " "

            let prefix = if s.fstatic then "" else "instance "

            sprintf "%s%s.%s %s : %s" prefix decl s.fname args (TypeRef.toString s.freturn)


    type FunctionDef =
        {
            finfo       : FunctionInfo
            fexpr       : Expr
        }

    module FunctionDef =
        open Patterns
        
        let toString (d : FunctionDef) =
            let prefix = if d.finfo.fstatic then "" else "instance "

            match d.fexpr with
                | Lambdas(_, b) ->
                    let b : Expr = b
                    let args =
                        d.finfo.fparameters |> List.map (fun block ->
                            block
                            |> List.map (fun (name, typ) -> sprintf "%s : %s" name (TypeRef.toString typ))
                            |> String.concat ", "
                            |> sprintf "(%s)"
                        )
                        |> String.concat " "
                    sprintf "%slet %s %s : %s = %s" prefix d.finfo.fname args (TypeRef.toString b.Type) (Expr.toString b)

                | b ->
                    sprintf "%slet %s : %s = %s" prefix d.finfo.fname (TypeRef.toString b.Type) (Expr.toString b)

        let ofExpr (name : string) (ex : Expr) =
            let args, body = 
                match ex with
                    | Lambdas(vs, b) -> vs, b
                    | _ -> [], ex
            
            let info =
                {
                    fdeclaring  = TypeInfo.Top
                    fname       = name
                    fgeneric    = []
                    fparameters = args |> List.map (fun (v : Var) -> [v.Name, v.Type])
                    freturn     = body.Type
                    fstatic     = true
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
        | ECall of f : FunctionInfo
        | ELambda of Var
        | ENewRecord of TypeRef
        | ESequential
        | EFieldGet of name : string * dst : TypeRef
        | EFieldSet of name : string * dst : TypeRef
        | ETupleGet of index : int
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

        static member Call(f : FunctionInfo, args : list<Expr>) = 
            let argTypes = args |> List.map (fun e -> e.Type)
            match TypeRef.tryApply argTypes f.ftype with
                | Some ret ->
                    Expr(ECall f, ret, args)
                | None ->
                    failwithf 
                        "[FShade] inconsistent argument types for %s: [%s]" 
                        (FunctionInfo.toString f) 
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

        static member TupleGet(v : Expr, i : int) =
            match v.Type with
                | RTuple es ->
                    match List.tryItem i es with
                    | Some e ->
                        Expr(ETupleGet i, e, [v])
                    | None ->
                        failwith "tuple-index out of bounds"
                | _ ->
                    failwith "not a tuple"

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

        let (|TupleGet|_|) (e : Expr) =
            match e.Info with
                | ETupleGet i -> Some(List.head e.Children, i)
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
                    sprintf "alloc<%s>" (TypeRef.toString t)

                | TupleGet(e, i) ->
                    sprintf "%s[[%d]]" (toString e) i

                | _ ->
                    failwith "unreachable"


    type Module =
        {
            mname       : string
            mtypes      : Map<TypeInfo, TypeDef>
            mfunctions  : Map<FunctionInfo, FunctionDef>
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
    open System.Threading
    open System.Reflection
    open Stuff
    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns

    type private TExpr = Stuff.Expr
    type private TVar = Stuff.Var

    [<Struct; CustomEquality; CustomComparison>]
    type TypeId private(id : int) =
        static let mutable current = 0
        static member New = TypeId(Interlocked.Increment(&current))

        member private x.Id = id

        interface IComparable with
            member x.CompareTo o =
                match o with
                | :? TypeId as o -> compare id o.Id
                | _ -> failwith "cannot compare"
                
        interface IComparable<TypeId> with
            member x.CompareTo o = compare id o.Id

        override x.ToString() = sprintf "T%d" id
        override x.GetHashCode() = id
        override x.Equals o =
            match o with
            | :? TypeId as o -> id = o.Id
            | _ -> false



    type TranslationState =
        {
            typeImps    : MapExt<string, Set<TypeInfo>>
            typeIds     : hmap<Type, TypeId>
            typeDefs    : hmap<Type, TypeDef>
            typeRefs    : hmap<Type, TypeRef>
            
            funImps     : MapExt<string, Set<FunctionInfo>>
            funRefs     : hmap<MethodBase, FunctionInfo>
            funDefs     : hmap<MethodBase, FunctionDef>

            variables   : MapExt<Var, TVar>
        }

        static member Empty = { typeIds = HMap.empty; typeImps = MapExt.empty; typeRefs = HMap.empty; typeDefs = HMap.empty; funImps = MapExt.empty; funRefs = HMap.empty; funDefs = HMap.empty; variables = MapExt.empty }

    module TranslationState =
        let importType (t : TypeInfo) (s : TranslationState) =
            { s with
                typeImps = MapExt.alter t.eassembly (Option.defaultValue Set.empty >> Set.add t >> Some) s.typeImps
            }

        let importFunction (t : FunctionInfo) (s : TranslationState) =
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
                mname = TypeInfo.Top.efullname
                mfunctions = s.funDefs |> HMap.toSeq |> Seq.map (fun (_,d) -> d.finfo, d) |> Map.ofSeq
                mtypes = s.typeDefs |> HMap.toSeq |> Seq.map (fun (_,d) -> d.dinfo, d) |> Map.ofSeq
            }
            


    module TypeInfo =
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
                    let! ref = TypeInfo.ofTypeS t

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
                            
                        let! ref = TypeInfo.ofTypeS t

                        
                        let def = { dinfo = ref; ddef = DUnion(cases) }
                        do! State.modify (TranslationState.defineType t def)
                        return Some def

                    else   
                        let isReflectable = 
                            t.GetMembers(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.CreateInstance ||| BindingFlags.Instance ||| BindingFlags.Static)
                            |> Seq.tryPick (function (:? MethodBase as m) -> Expr.TryGetReflectedDefinition m | _ -> None)
                            |> Option.isSome
                            
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
                            let! ref = TypeInfo.ofTypeS t
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
        open Microsoft.FSharp.Quotations.ExprShape

        let rec removeUnitValue (e : Expr) =
            match e with
                | Sequential(l, Unit) -> removeUnitValue l
                | Sequential(Unit, r) -> removeUnitValue r
                | ShapeCombination(o, args) -> RebuildShapeCombination(o, List.map removeUnitValue args)
                | ShapeLambda(v,b) -> Expr.Lambda(v, removeUnitValue b)
                | ShapeVar _ -> e
                    
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

                    let body = removeUnitValue body

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
                    e

        let private getParameters (e : Expr) =
            match e with
                | Lambdas(vs, _) ->
                    vs
                | _ ->
                    []

        let rec private detuple (e : Expr) =
            match e with
                | Lambdas(vs, body) ->
                    let vs = vs |> List.concat

                    
                    let rec wrap (vs : list<Var>) (e : Expr) =
                        match vs with
                            | [] -> e
                            | h :: t -> Expr.Lambda(h, wrap t e)
                    wrap vs body
                | _ ->
                    e

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
                                    
                                let! e = TExpr.ofExpr (detuple expr)
                                
                                let! info = FunctionInfo.tryOfMethodS false m
                                let info : FunctionInfo = Option.get info
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
                                            fdeclaring = TypeInfo.Top
                                            fparameters = pars
                                    }

                                let res =
                                    {
                                        finfo = info
                                        fexpr = e
                                    }
                                
                                do! State.modify (fun s ->
                                        let update (o : Option<Set<FunctionInfo>>) =
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
                    | None -> return! FunctionInfo.ofMethodS m
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
                                let! e = TypeInfo.ofTypeS t
                                return RImported(e, None)
                            
            }

    module FunctionInfo =

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
                        let! d = TypeInfo.ofTypeS mb.DeclaringType
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
                                    fstatic     = mb.IsStatic
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
                                    fstatic     = true
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

                    | TupleGet(e, i) ->
                        let! e = ofExpr e
                        return TExpr.TupleGet(e, i)

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
    let b = 2 * a
    member x.A = a
    member x.Bla(r : int, y :  int) = r*y + x.A * b
[<ReflectedDefinition>]
let test (a : int) (b : int) = a + b

let ex = <@ fun (a : int) -> A(a).Bla(12, 13) @>
