#load "load.fsx"

open Aardvark.Base
open System.Runtime.CompilerServices
open Aardvark.Base.Rendering
open Microsoft.FSharp.Quotations
open Aardvark.Rendering.Vulkan

type Marker = Marker
do
    let dir = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "obj")
    let info = System.IO.DirectoryInfo dir
    if not info.Exists then info.Create()
    System.AppDomain.CurrentDomain.SetData("APPBASE", dir)
    System.Environment.CurrentDirectory <- dir
    Ag.initialize()
    Aardvark.Init()

type Num<'a> =
    {
        zero : 'a
        one : 'a
        add : 'a -> 'a -> 'a
        sub : 'a -> 'a -> 'a
        mul : 'a -> 'a -> 'a
        div : 'a -> 'a -> 'a
        neg : 'a -> 'a
        pow : 'a -> int -> 'a
        fromInt : int -> 'a
        fromFloat : float -> 'a
        isTiny : 'a -> bool
        isTinyEps : float -> 'a -> bool
        isPositive : 'a -> bool
    }

module NumInstances =
    
    let Cint32 =
        {
            zero = 0
            one = 1
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> pown v n
            fromInt = id
            fromFloat = int
            isTiny = fun v -> v = 0
            isTinyEps = fun _ v -> v = 0
            isPositive = fun v -> v > 0
        }
   
    let Cint64 =
        {
            zero = 0L
            one = 1L
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> pown v n
            fromInt = int64
            fromFloat = int64
            isTiny = fun v -> v = 0L
            isTinyEps = fun _ v -> v = 0L
            isPositive = fun v -> v > 0L
        }

    let Cfloat32 =
        {
            zero = 0.0f
            one = 1.0f
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> pown v n
            fromInt = float32
            fromFloat = float32
            isTiny = Fun.IsTiny 
            isTinyEps = fun e v -> Fun.IsTiny(v, float32 e)
            isPositive = fun v -> v > 0.0f
        }

    let Cfloat64 =
        {
            zero = 0.0
            one = 1.0
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> pown v n
            fromInt = float
            fromFloat = float
            isTiny = Fun.IsTiny 
            isTinyEps = fun e v -> Fun.IsTiny(v, e)
            isPositive = fun v -> v > 0.0
        }

    let CV2i =
        {
            zero = V2i.Zero
            one = V2i.II
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V2i(pown v.X n, pown v.Y n)
            fromInt = fun v -> V2i(v,v)
            fromFloat = fun v -> V2i(int v, int v)
            isTiny = fun v -> v = V2i.Zero
            isTinyEps = fun _ v -> v = V2i.Zero
            isPositive = fun v -> v.AllGreater 0
        }
        
    let CV2l =
        {
            zero = V2l.Zero
            one = V2l.II
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V2l(pown v.X n, pown v.Y n)
            fromInt = fun v -> V2l(int64 v, int64 v)
            fromFloat = fun v -> V2l(int64 v, int64 v)
            isTiny = fun v -> v = V2l.Zero
            isTinyEps = fun _ v -> v = V2l.Zero
            isPositive = fun v -> v.AllGreater 0L
        }

    let CV2f =
        {
            zero = V2f.Zero
            one = V2f.II
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V2f(pown v.X n, pown v.Y n)
            fromInt = fun v -> V2f(float32 v, float32 v)
            fromFloat = fun v -> V2f(float32 v, float32 v)
            isTiny = fun v -> Fun.IsTiny(v.X) && Fun.IsTiny(v.Y)
            isTinyEps = fun e v -> Fun.IsTiny(v.X, float32 e) && Fun.IsTiny(v.Y, float32 e)
            isPositive = fun v -> v.AllGreater 0.0f
        }

    let CV2d =
        {
            zero = V2d.Zero
            one = V2d.II
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V2d(pown v.X n, pown v.Y n)
            fromInt = fun v -> V2d(float v, float v)
            fromFloat = fun v -> V2d(v, v)
            isTiny = fun v -> Fun.IsTiny(v.X) && Fun.IsTiny(v.Y)
            isTinyEps = fun e v -> Fun.IsTiny(v.X, e) && Fun.IsTiny(v.Y, e)
            isPositive = fun v -> v.AllGreater 0.0
        }


    let CV3i =
        {
            zero = V3i.Zero
            one = V3i.III
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V3i(pown v.X n, pown v.Y n, pown v.Z n)
            fromInt = fun v -> V3i(v,v,v)
            fromFloat = fun v -> V3i(int v, int v, int v)
            isTiny = fun v -> v = V3i.Zero
            isTinyEps = fun _ v -> v = V3i.Zero
            isPositive = fun v -> v.AllGreater 0
        }
  
    let CV3l =
        {
            zero = V3l.Zero
            one = V3l.III
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V3l(pown v.X n, pown v.Y n, pown v.Z n)
            fromInt = fun v -> V3l(int64 v, int64 v, int64 v)
            fromFloat = fun v -> V3l(int64 v, int64 v, int64 v)
            isTiny = fun v -> v = V3l.Zero
            isTinyEps = fun _ v -> v = V3l.Zero
            isPositive = fun v -> v.AllGreater 0L
        }
  
    let CV3f =
        {
            zero = V3f.Zero
            one = V3f.III
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V3f(pown v.X n, pown v.Y n, pown v.Z n)
            fromInt = fun v -> V3f(float32 v, float32 v, float32 v)
            fromFloat = fun v -> V3f(float32 v, float32 v, float32 v)
            isTiny = fun v -> Fun.IsTiny(v.X) && Fun.IsTiny(v.Y) && Fun.IsTiny(v.Z)
            isTinyEps = fun e v -> Fun.IsTiny(v.X, float32 e) && Fun.IsTiny(v.Y, float32 e) && Fun.IsTiny(v.Z, float32 e)
            isPositive = fun v -> v.AllGreater 0.0f
        }
     
    let CV3d =
        {
            zero = V3d.Zero
            one = V3d.III
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V3d(pown v.X n, pown v.Y n, pown v.Z n)
            fromInt = fun v -> V3d(float v, float v, float v)
            fromFloat = fun v -> V3d(v, v, v)
            isTiny = fun v -> Fun.IsTiny(v.X) && Fun.IsTiny(v.Y) && Fun.IsTiny(v.Z)
            isTinyEps = fun e v -> Fun.IsTiny(v.X, e) && Fun.IsTiny(v.Y, e) && Fun.IsTiny(v.Z, e)
            isPositive = fun v -> v.AllGreater 0.0
        }

    let CV4i =
        {
            zero = V4i.Zero
            one = V4i.IIII
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V4i(pown v.X n, pown v.Y n, pown v.Z n, pown v.W n)
            fromInt = fun v -> V4i(v,v,v,v)
            fromFloat = fun v -> V4i(int v, int v, int v, int v)
            isTiny = fun v -> v = V4i.Zero
            isTinyEps = fun _ v -> v = V4i.Zero
            isPositive = fun v -> v.AllGreater 0
        }
  
    let CV4l =
        {
            zero = V4l.Zero
            one = V4l.IIII
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V4l(pown v.X n, pown v.Y n, pown v.Z n, pown v.W n)
            fromInt = fun v -> V4l(v,v,v,v)
            fromFloat = fun v -> V4l(int64 v, int64 v, int64 v, int64 v)
            isTiny = fun v -> v = V4l.Zero
            isTinyEps = fun _ v -> v = V4l.Zero
            isPositive = fun v -> v.AllGreater 0L
        }
  
    let CV4f =
        {
            zero = V4f.Zero
            one = V4f.IIII
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V4f(pown v.X n, pown v.Y n, pown v.Z n, pown v.W n)
            fromInt = fun v -> V4f(float32 v, float32 v, float32 v, float32 v)
            fromFloat = fun v -> V4f(float32 v, float32 v, float32 v, float32 v)
            isTiny = fun v -> Fun.IsTiny v.X && Fun.IsTiny v.Y && Fun.IsTiny v.Z && Fun.IsTiny v.W
            isTinyEps = fun e v -> Fun.IsTiny(v.X, float32 e) && Fun.IsTiny(v.Y, float32 e) && Fun.IsTiny(v.Z, float32 e) && Fun.IsTiny(v.W, float32 e)
            isPositive = fun v -> v.AllGreater 0.0f
        }

    let CV4d =
        {
            zero = V4d.Zero
            one = V4d.IIII
            add = (+)
            sub = (-)
            mul = (*)
            div = (/)
            neg = (~-)
            pow = fun v n -> V4d(pown v.X n, pown v.Y n, pown v.Z n, pown v.W n)
            fromInt = fun v -> V4d(float v, float v, float v, float v)
            fromFloat = fun v -> V4d(v, v, v, v)
            isTiny = fun v -> Fun.IsTiny v.X && Fun.IsTiny v.Y && Fun.IsTiny v.Z && Fun.IsTiny v.W
            isTinyEps = fun e v -> Fun.IsTiny(v.X, e) && Fun.IsTiny(v.Y, e) && Fun.IsTiny(v.Z, e) && Fun.IsTiny(v.W, e)
            isPositive = fun v -> v.AllGreater 0.0
        }

    let internal table =
        LookupTable.lookupTable [
            typeof<int>,        Cint32 :> obj
            typeof<int64>,      Cint64 :> obj
            typeof<float32>,    Cfloat32 :> obj
            typeof<float>,      Cfloat64 :> obj
            typeof<V2i>,        CV2i :> obj
            typeof<V2l>,        CV2l :> obj
            typeof<V2f>,        CV2f :> obj
            typeof<V2d>,        CV2d :> obj
            typeof<V3i>,        CV3i :> obj
            typeof<V3l>,        CV3l :> obj
            typeof<V3f>,        CV3f :> obj
            typeof<V3d>,        CV3d :> obj
            typeof<V4i>,        CV4i :> obj
            typeof<V4l>,        CV4l :> obj
            typeof<V4f>,        CV4f :> obj
            typeof<V4d>,        CV4d :> obj
        ]

    let instance<'a> = table typeof<'a> |> unbox<Num<'a>>

[<AutoOpen>]
module private PolynomialHelpers =

    let arr2d (arr : float[][]) =
        let r = arr.Length
        if r = 0 then 
            Array2D.zeroCreate 0 0
        else
            let c = arr.[0].Length
            Array2D.init r c (fun r c -> arr.[r].[c])

    let inline doWhile (cond : unit -> bool) (action : unit -> unit) =
        action()
        while cond() do action()


    let cross<'p, 'c when 'p : comparison> (num : Num<'c>) (l : MapExt<MapExt<string * 'p, int>, 'c>) (r : MapExt<MapExt<string * 'p, int>, 'c>) : MapExt<MapExt<string * 'p, int>, 'c> =
        let l = l |> MapExt.toList
        let r = r |> MapExt.toList

        let inline union (l : int) (r : int) =
            l + r

        let mutable res = MapExt.empty
        for ((lk, lv), (rk, rv)) in List.allPairs l r do
            let k = MapExt.unionWith union lk rk
            let v = num.mul lv rv
            if not (num.isTiny v) then
                res <- MapExt.alter k (Option.defaultValue num.zero >> num.add v >> Some) res
        res

    let inline toOption (v : float) =
        if Fun.IsTiny v then None
        else Some v

    module List =
        let inline mulBy (f : 'a -> 'b) (l : list<'a>) =
            let mutable res = LanguagePrimitives.GenericOne
            for e in l do
                res <- res * f e
            res

            
    module Seq =
        let inline mulBy (f : 'a -> 'b) (l : seq<'a>) =
            let mutable res = LanguagePrimitives.GenericOne
            for e in l do
                res <- res * f e
            res


[<StructuredFormatDisplay("{AsString}")>]
type Polynomial<'p, 'c when 'p : comparison> (coeff : MapExt<MapExt<string * 'p, int>, 'c>) =

    let names =  
        lazy ( coeff |> MapExt.toSeq |> Seq.collect (fst >> MapExt.toSeq >> Seq.map (fst >> fst)) |> Set.ofSeq )

    let degrees =
        lazy (
            if MapExt.isEmpty coeff then
                MapExt.empty
            else
                names.Value |> Seq.map (fun name ->
                    name, lazy (coeff |> MapExt.toSeq |> Seq.map (fun (k,_) -> k |> MapExt.toSeq |> Seq.filter (fun ((n,_),_) -> n = name) |> Seq.sumBy snd) |> Seq.max)
                )
                |> MapExt.ofSeq
        )

    let degree (name : string) = 
        match MapExt.tryFind name degrees.Value with
            | Some v -> v.Value
            | None -> 0

    static let num = NumInstances.instance<'c>

    static let (<+>) (l : 'c) (r : 'c) = num.add l r
    static let (<*>) l r = num.mul l r

    static let toOption v = if num.isTiny v then None else Some v

    member x.Degree(name : string) = degree name
    member x.coefficients = coeff

    static member private map (f : 'c -> 'c) (l : Polynomial<'p, 'c>) =
        let merge _ (l : 'c) = 
            let r = f l
            if num.isTiny r then None
            else Some r

        Polynomial(MapExt.choose merge l.coefficients)

    static member private map2 (f : 'c -> 'c -> 'c) (l : Polynomial<'p, 'c>) (r : Polynomial<'p, 'c>)=
        let merge _ (l : Option<'c>) (r : Option<'c>) =
            match l with
                | Some l ->
                    match r with
                        | Some r -> f l r |> toOption
                        | None -> f l num.zero |> toOption
                | None ->
                    match r with
                        | Some r -> f num.zero r |> toOption
                        | None -> f num.zero num.zero |> toOption
        Polynomial(MapExt.choose2 merge l.coefficients r.coefficients)
        
    static member Constant v = Polynomial<'p, 'c> (MapExt.ofList [ MapExt.empty, v ])
    static member Zero = Polynomial<'p, 'c> (MapExt.empty)
    static member One = Polynomial<'p, 'c> (MapExt.ofList [ MapExt.empty, num.one ])
    
    static member (~-) (l : Polynomial<'p, 'c>) = Polynomial<'p, 'c>.map num.neg l
    
    static member (+) (l : Polynomial<'p, 'c>, r : Polynomial<'p, 'c>) = Polynomial<'p, 'c>.map2 num.add l r

    static member (+) (l : 'c, r : Polynomial<'p, 'c>) = 
        if num.isTiny l then
            r
        else
            Polynomial(MapExt.alter MapExt.empty (fun o -> match o with | None -> Some l | Some r -> toOption (l <+> r)) r.coefficients )

    static member (+) (l : Polynomial<'p, 'c>, r : 'c) = 
        if num.isTiny r then
            l
        else
            Polynomial(MapExt.alter MapExt.empty (fun o -> match o with | None -> Some r | Some l -> toOption (l <+> r)) l.coefficients )
       

    static member (-) (l : Polynomial<'p, 'c>, r : Polynomial<'p, 'c>) = Polynomial<'p, 'c>.map2 num.sub l r

    static member (-) (l : 'c, r : Polynomial<'p, 'c>) = Polynomial<'p, 'c>.Constant(l) - r
    static member (-) (l : Polynomial<'p, 'c>, r : 'c) = l + Polynomial<'p, 'c>.Constant (num.neg r)

    static member (*) (l : Polynomial<'p, 'c>, r : Polynomial<'p, 'c>) = Polynomial(cross num l.coefficients r.coefficients)
      
    static member (*) (l : Polynomial<'p, 'c>, r : 'c) = l * Polynomial<'p, 'c>.Constant r
    static member (*) (l : 'c, r : Polynomial<'p, 'c>) = Polynomial<'p, 'c>.Constant l * r

    static member Pow(l : Polynomial<'p, 'c>, r : int) =
        if r < 0 then failwith "negative exponent"
        elif r = 0 then Polynomial<'p,'c>.Zero
        elif r = 1 then l
        else l * Polynomial<'p,'c>.Pow(l, r - 1)

    override x.ToString() =
        if MapExt.isEmpty x.coefficients then
            "0"
        else
            let paramStr (p : MapExt<string * 'p, int>) =
                p |> MapExt.toSeq |> Seq.map (fun ((name,p),e) -> 
                    let p = 
                        if typeof<'p> = typeof<int> then sprintf "(%A)" p
                        else sprintf "%A" p
                    if e = 1 then
                        sprintf "%s%s" name p
                    else
                        sprintf "%s%s^%d" name p e
                )
                |> String.concat "*"
        
            x.coefficients 
            |> MapExt.toSeq 
            |> Seq.sortByDescending (fun (p,_) -> p |> MapExt.toSeq |> Seq.sumBy snd)
            |> Seq.mapi (fun i (p,f) ->
                let op, f = 
                    if i = 0 then
                        "", f
                    elif num.isPositive f then
                        " + ", f
                    else
                        " - ", num.neg f
                    
                if MapExt.isEmpty p then
                    sprintf "%s%A" op f
                else
                    let isOne = num.isTiny (num.sub f num.one)
                    let isMinusOne = num.isTiny (num.add f num.one)
                    
                    let p = paramStr p
                    if isOne then
                        sprintf "%s%s" op p

                    elif isMinusOne then
                        sprintf "%s-%s" op p

                    else
                        sprintf "%s%A*%s" op f p
            )
            |> String.concat ""

    member private x.AsString = x.ToString()

    member x.Evaluate(v : MapExt<string * 'p, 'c>) =
        x.coefficients 
        |> MapExt.toSeq
        |> Seq.fold (fun s (k,f) ->
            let factor = 
                k |> MapExt.toSeq |> Seq.fold (fun r (p,e) -> 
                    match MapExt.tryFind p v with
                        | Some v -> num.pow v e
                        | _ -> num.zero
                ) num.one

            s <+> (factor <*> f)
        ) num.zero

    member x.Derivative(name : string, p : 'p) =
        let p = (name,p)
        let mutable coeff = MapExt.empty
        for (c, v) in MapExt.toSeq x.coefficients do
             match MapExt.tryFind p c with
                    | Some e ->
                        if e = 1 then
                            let c' = MapExt.remove p c
                            let v' = v <*> (num.fromInt e)
                            coeff <- MapExt.alter c' (function Some o -> toOption (v' <+> o) | None -> toOption v') coeff
                        else
                            let c' = MapExt.add p (e-1) c
                            let v' = v <*> (num.fromInt e)
                            coeff <- MapExt.alter c' (function Some o -> toOption (v' <+> o) | None -> toOption v') coeff

                            
                    | None ->
                        ()

        Polynomial(coeff)

    member x.FreeParameters =
        let mutable res = MapExt.empty
        let all = x.coefficients |> MapExt.toSeq |> Seq.collect (fun (k,_) -> k |> MapExt.toSeq |> Seq.map (fun (k,_) -> k)) 
        for (name, pi) in all do
            res <- MapExt.alter name (fun s -> s |> Option.defaultValue Set.empty |> Set.add pi |> Some) res

        res

    member x.AllDerivatives(name : string) =
        match MapExt.tryFind name x.FreeParameters with
            | Some parameters -> 
                parameters |> Seq.map (fun p ->
                    p, x.Derivative(name, p)
                )
                |> MapExt.ofSeq
            | None ->
                MapExt.empty
        
    member x.AllSecondDerivatives(name : string) =
        match MapExt.tryFind name x.FreeParameters with
            | Some free -> 
                Seq.allPairs free free 
                |> Seq.choose (fun (p0, p1) ->
                    let d = x.Derivative(name, p0).Derivative(name, p1)
                    if MapExt.isEmpty d.coefficients then
                        None
                    else
                        Some ((p0, p1), d)
                )
                |> MapExt.ofSeq
            | None ->
                MapExt.empty

[<AutoOpen>]
module PolynomialExtensions =

    [<Struct>]
    type PolynomialParam<'a> (name : string)=
        static let num = NumInstances.instance<'a>

        member x.Item
            with inline get (i : int) = Polynomial<int, 'a> ( MapExt.ofList [ MapExt.ofList [(name, i), 1], num.one ] )

        member x.Item
            with inline get (i : int, j : int) = Polynomial<int * int, 'a> (MapExt.ofList [ MapExt.ofList [(name, (i,j)), 1], num.one ] )
        

    [<GeneralizableValue>]
    let x<'a> = PolynomialParam<'a> "x"
    
    [<GeneralizableValue>]
    let y<'a> = PolynomialParam<'a> "y"

    [<GeneralizableValue>]
    let z<'a> = PolynomialParam<'a> "z"

    [<GeneralizableValue>]
    let w<'a> = PolynomialParam<'a> "w"
    
    [<AllowNullLiteral>]
    type Param private() = class end
    let param : Param = null
    let inline (?) (v : Param) (name : string) : 'p -> Polynomial<'p, 'c> =
        let num = NumInstances.instance<'c>
        fun i -> Polynomial<'p, 'c> ( MapExt.ofList [ MapExt.ofList [(name, i), 1], num.one ] )

module Polynomial =

    [<GeneralizableValue>]
    let zero<'p, 'c when 'p : comparison> = Polynomial<'p, 'c>.Zero

    [<GeneralizableValue>]
    let one<'p, 'c when 'p : comparison> = Polynomial<'p, 'c>.One

    let inline evaluate (values : MapExt<string * 'p, 'c>) (p : Polynomial<'p, 'c>) = p.Evaluate(values)


    type Data<'c, 'a, 'v> =
        {
            num     : Num<'v>
            dim     : 'a -> 'c
            fetch   : 'c -> 'a -> 'v
            init    : 'c -> ('c -> 'v) -> 'a
            iter    : 'c -> ('c -> unit) -> unit
            zero    : 'c
            add     : 'c -> 'c -> 'c
            clamp   : 'c -> 'c -> 'c -> 'c
        }

        member x.map (f : 'v -> 'v) (e : 'a) =
            let d = x.dim e
            x.init d (fun c ->
                f (x.fetch c e)
            )
            
        member x.dot (l : 'a) (r : 'a) =
            let d = x.dim l
            let mutable sum = x.num.zero
            x.iter d (fun c ->
                let s = x.num.mul (x.fetch c l) (x.fetch c r)
                sum <- x.num.add sum s
            )
            sum

        member x.mulS (s : 'v) (v : 'a) =
            let d = x.dim v
            x.init d (fun c ->
                x.num.mul s (x.fetch c v)
            )
            
        member x.mad (a : 'a) (s : 'v) (b : 'a) =
            let d = x.dim a
            x.init d (fun c ->
                x.num.add (x.fetch c a) (x.num.mul s (x.fetch c b))
            )

    let cg<'c, 'a, 'v> (data : Data<'c, 'a, 'v>) (eps : float) (f' : 'a -> 'a) (d2MulVDotV : 'a -> 'a -> 'v) (x : 'a) =
    
        let n = 15
        let imax = 50
        let jmax = 10

        let mutable i = 0
        let mutable j = 0
        let mutable k = 0
        let mutable r = data.map data.num.neg (f' x)
        let mutable d = r
        let mutable deltaOld = data.num.zero
        let mutable deltaNew = data.dot r r
        let mutable di = deltaNew
        let mutable x = x
        let mutable alpha = data.num.zero

        let eps2 = eps * eps
        while i < imax && not (data.num.isTinyEps eps2 deltaNew) do
            //printfn "r = %e" (sqrt deltaNew)
            j <- 0
            di <- data.dot d d 


            doWhile (fun () -> j < jmax && not (data.num.isTinyEps eps2 (data.num.mul alpha di)) ) (fun () -> //alpha*di  > eps2) (fun () ->
                let alpha = data.num.neg (data.num.div (data.dot (f' x) d) (d2MulVDotV x d)) //(dot d (f'' x d))
                x <- data.mad x alpha d //add x (data.mulS alpha d)
                //printfn "x = %A" x
                j <- j + 1
            )

            r <- data.map data.num.neg (f' x)
            deltaOld <- deltaNew
            deltaNew <- data.dot r r
            let beta = data.num.div deltaNew deltaOld
            d <- data.mad r beta d //add r (data.mulS beta d)
            k <- k + 1
            if k = n || data.num.neg(data.dot r d) |> data.num.isPositive then
                d <- r
                k <- 0

            i <- i + 1

        
        //printfn "iter = %A" i
        x


    let cgMinimize<'c, 'a, 'v when 'c : comparison> (data : Data<'c, 'a, 'v>) (eps : float) (name : string) (p : Polynomial<'c, 'v>) (x0 : 'a) =
        let num = data.num

        let p' = p.Derivative(name, data.zero)
        let p'' = p'.AllDerivatives(name)

        let bakedDerivative = 
            MapExt.toArray p'.coefficients
                |> Array.collect (fun (k,f) ->
                    if MapExt.isEmpty k then
                        [| (f, data.zero, 0) |]
                    else
                        k |> MapExt.toArray |> Array.map (fun ((_,o), e) ->
                            (f,o,e)
                        )
                )
        
        let bakedSecondDerivative = 
            MapExt.toArray p''
                |> Array.collect (fun (c, p) ->
                    p.coefficients |> MapExt.toArray |> Array.collect (fun (par, f) ->
                        if MapExt.isEmpty par then
                            [| (f, c, data.zero, 0) |]
                        else
                            par |> MapExt.toArray |> Array.map (fun ((_,o), e) ->
                                (f,c,o,e)
                            )
                    )
                )
                
        let d (x : 'a) =
            data.init (data.dim x) (fun i ->
                bakedDerivative |> Array.fold (fun s (f,o,e) ->
                    let o = data.add i o
                    let o = data.clamp data.zero (data.dim x) o
                    let value = 
                        if e = 0 then
                            f 

                        elif e = 1 then
                            num.mul f (data.fetch o x)
                        
                        else
                            num.mul f (num.pow (data.fetch o x) e)

                    num.add s value
                ) num.zero
            )

        let d2MulVDotV (x : 'a) (v : 'a) =
            let mutable sum = num.zero
            
            data.iter (data.dim v) (fun rd ->
                let res =
                    bakedSecondDerivative |> Array.fold (fun s (f,c,o,e) ->
                        let value = 
                            let c = data.add rd c
                            let c = data.clamp data.zero (data.dim v) c
                            
                            let r = data.add rd o
                            let r = data.clamp data.zero (data.dim x) r
                            //let r = rd + o
                            //let r = clamp 0 (x.Length - 1) r

                            if e = 0 then
                                num.mul (data.fetch c v) (num.mul f (data.fetch r v))
                            else
                                num.mul (num.mul (data.fetch c v) (num.mul f (num.pow (data.fetch r x) e))) (data.fetch r v)
                                //v.[c] * (f * x.[r] ** float e) * v.[r]

                        num.add s value
                    ) num.zero
                sum <- num.add sum res
            )

            sum

        cg data eps d d2MulVDotV x0



[<Extension; AbstractClass; Sealed>]
type CG private() =
    [<Extension>]
    static member Solve(p : Polynomial<int, 'v>, name : string, x0 : 'v[], eps : float) =
        let data : Polynomial.Data<int, 'v[], 'v> = 
            {
                num     = NumInstances.instance<'v>
                dim     = Array.length
                fetch   = Array.item
                init    = Array.init
                iter    = fun (c : int) f -> for i in 0 .. c - 1 do f i
                zero    = 0
                clamp   = fun l h v -> clamp l (h - 1) v
                add     = (+)
            }

        Polynomial.cgMinimize data eps name p x0
        
    [<Extension>]
    static member Solve(p : Polynomial<int * int, 'v>, name : string, x0 : 'v[,], eps : float) =
        let data : Polynomial.Data<int * int, 'v[,], 'v> = 
            {
                num     = NumInstances.instance<'v>
                dim     = fun a -> a.GetLength(0), a.GetLength(1)
                fetch   = fun (i,j) a -> a.[i,j]
                init    = fun (r,c) f -> Array2D.init r c (fun r c -> f(r,c))
                iter    = fun (r, c) f -> for i in 0 .. r - 1 do for j in 0 .. c - 1 do f(i,j)
                zero    = (0,0)
                clamp   = fun (lx, ly) (hx, hy) (vx, vy) -> (clamp lx (hx - 1) vx, clamp ly (hy - 1) vy)
                add     = fun (lx, ly) (hx, hy) -> (lx + hx, ly + hy)
            }

        Polynomial.cgMinimize data eps name p x0
        
    [<Extension>]
    static member Solve(p : Polynomial<int * int, 'v>, name : string, x0 : Matrix<'v>, eps : float) =
        let data : Polynomial.Data<int * int, Matrix<'v>, 'v> = 
            {
                num     = NumInstances.instance<'v>
                dim     = fun a -> (int a.Size.X, int a.Size.Y)
                fetch   = fun (x,y) a -> a.[x,y]
                init    = fun (x,y) f -> Matrix<'v>(V2l(x,y)).SetByCoord(fun (c : V2l) -> f(int c.X, int c.Y))
                iter    = fun (sx, sy) f -> for i in 0 .. sx - 1 do for j in 0 .. sy - 1 do f (i,j)
                zero    = (0,0)
                clamp   = fun (lx, ly) (hx, hy) (vx, vy) -> (clamp lx (hx - 1) vx, clamp ly (hy - 1) vy)
                add     = fun (lx, ly) (hx, hy) -> (lx + hx, ly + hy)
            }

        Polynomial.cgMinimize data eps name p x0
        
    [<Extension>]
    static member Solve(p : Polynomial<int, 'v>, name : string, x0 : Vector<'v>, eps : float) =
        let data : Polynomial.Data<int, Vector<'v>, 'v> = 
            {
                num     = NumInstances.instance<'v>
                dim     = fun a -> int a.Size
                fetch   = fun (x) a -> a.[x]
                init    = fun (x) f -> Vector<'v>(x).SetByCoord(fun (c : int64) -> f(int c))
                iter    = fun (sx) f -> for i in 0 .. sx - 1 do f i
                zero    = 0
                clamp   = fun (lx) (hx) (vx) -> (clamp lx (hx - 1) vx)
                add     = (+)
            }

        Polynomial.cgMinimize data eps name p x0

    [<Extension>]
    static member Solve(p : Polynomial<int, 'v>, x0 : 'v[], eps : float) =
        CG.Solve(p, "x", x0, eps)

    [<Extension>]
    static member Solve(p : Polynomial<int * int, 'v>, x0 : 'v[,], eps : float) =
        CG.Solve(p, "x", x0, eps)
        
    [<Extension>]
    static member Solve(p : Polynomial<int, 'v>, x0 : Vector<'v>, eps : float) =
        CG.Solve(p, "x", x0, eps)

    [<Extension>]
    static member Solve(p : Polynomial<int * int, 'v>, x0 : Matrix<'v>, eps : float) =
        CG.Solve(p, "x", x0, eps)

    [<Extension>]
    static member Solve(p : Polynomial<int, 'v>, x0 : 'v[]) =
        CG.Solve(p, "x", x0, 1E-8)
        
    [<Extension>]
    static member Solve(p : Polynomial<int * int, 'v>, x0 : 'v[,]) =
        CG.Solve(p, "x", x0, 1E-8)
        
    [<Extension>]
    static member Solve(p : Polynomial<int, 'v>, x0 : Vector<'v>) =
        CG.Solve(p, "x", x0, 1E-8)
        
    [<Extension>]
    static member Solve(p : Polynomial<int * int, 'v>, x0 : Matrix<'v>) =
        CG.Solve(p, "x", x0, 1E-8)

let grad = 0.25 * (-x<float>.[-1] + 2.0*x.[0] - x.[1]) ** 2
let grad2d = 0.125 * (4.0*x<float>.[0,0] - x.[1,0] - x.[-1,0] - x.[0,1] - x.[0,-1]) ** 2

let x02d =
    arr2d [|
        [| 1.0; 1.0; 1.0; 1.0; 1.0 |]
        [| 1.0; 3.0; 5.0; 3.0; 1.0 |]
        [| 1.0; 5.0; 9.0; 5.0; 1.0 |]
        [| 1.0; 3.0; 5.0; 3.0; 1.0 |]
        [| 1.0; 1.0; 1.0; 1.0; 1.0 |]
    |]

let x0 = [| 1.0; 2.0; 3.0; 2.0; 1.0 |]

let solve1d() =
    grad.Solve x0

let solve2d() =
    grad2d.Solve x02d


type RNum<'a> =
    {
        zero    : Expr<'a>
        one     : Expr<'a>
        add     : Expr<'a -> 'a -> 'a>
        mul     : Expr<'a -> 'a -> 'a>
    }

module ReflectedNum =
    let Cfloat64 =
        {
            zero    = <@ 0.0 @>
            one     = <@ 1.0 @>
            add     = <@ (+) @>
            mul     = <@ (*) @>
        }

    let Cfloat32 =
        {
            zero    = <@ 0.0f @>
            one     = <@ 1.0f @>
            add     = <@ (+) @>
            mul     = <@ (*) @>
        }

    let internal table =
        LookupTable.lookupTable [
            typeof<float32>,    Cfloat32 :> obj
            typeof<float>,      Cfloat64 :> obj
        ]

    let instance<'a> = table typeof<'a> |> unbox<RNum<'a>>

module CgSolverShader =
    open FShade

    [<Literal>]
    let foldSize = 128
    
    [<Literal>]
    let halfFoldSize = 64


    [<LocalSize(X = halfFoldSize)>]
    let dot1d (zero : Expr<'b>) (mul : Expr<'a -> 'a -> 'b>) (add : Expr<'b -> 'b -> 'b>) (cnt : int) (l : 'a[]) (r : 'a[]) (result : 'b[]) =
        compute {
            let mem = allocateShared<'b> foldSize
            let tid = getLocalId().X
            let gid = getWorkGroupId().X
            
            // index calculations
            let lai = 2 * tid
            let lbi = lai + 1
            let ai  = foldSize * gid + lai
            let bi  = ai + 1 
            
            // load existing values into local memory
            mem.[lai] <- if ai < cnt then (%mul) l.[ai] r.[ai] else %zero
            mem.[lbi] <- if bi < cnt then (%mul) l.[bi] r.[bi] else %zero
            barrier()
            
            // sum the local values from right to left
            let mutable s = halfFoldSize
            while s > 0 do
                if tid < s then
                    mem.[tid] <- (%add) mem.[tid] mem.[tid + s]

                s <- s >>> 1
                barrier()
                
            // store the overall sum in the result-buffer
            if tid = 0 then
                result.[gid] <- mem.[0]

        }
    

    let lSampler =
        sampler2d {
            texture uniform?l
            addressU WrapMode.Border
            addressV WrapMode.Border
            filter Filter.MinMagLinear
        }

    let rSampler =
        sampler2d {
            texture uniform?r
            addressU WrapMode.Border
            addressV WrapMode.Border
            filter Filter.MinMagLinear
        }

    [<LocalSize(X = 8, Y = 8)>]
    let sum2d (zero : Expr<'b>) (addV4 : Expr<V4d -> V4d -> 'b>) (add : Expr<'b -> 'b -> 'b>) (lLevel : int) (result : 'b[]) =
        compute {
            let mem = allocateShared<'b> 128

            let size = lSampler.GetSize(lLevel)
            let rcpSize = 1.0 / V2d size

            let lid = getLocalId().XY
            let gid = getWorkGroupId().XY
            let groups = getWorkGroupCount().XY

            // index calculations
            let id = gid * 16 + lid * 2
            let tc00 = (V2d id + V2d.Half) * rcpSize
            let tc01 = tc00 + V2d(rcpSize.X, 0.0)
            let tc10 = tc00 + V2d(0.0, rcpSize.Y)
            let tc11 = tc00 + rcpSize
            let tid = lid.X + 8 * lid.Y

            // load existing values into local memory
            let v0 =
                (%addV4) (lSampler.SampleLevel(tc00, float lLevel)) (lSampler.SampleLevel(tc01, float lLevel))
                
            let v1 =
                (%addV4) (lSampler.SampleLevel(tc10, float lLevel)) (lSampler.SampleLevel(tc11, float lLevel)) 

            mem.[tid * 2 + 0] <- v0
            mem.[tid * 2 + 1] <- v1
            barrier()
            
            // sum the local values from right to left
            let mutable s = 64
            while s > 0 do
                if tid < s then
                    mem.[tid] <- (%add) mem.[tid] mem.[tid + s]

                s <- s >>> 1
                barrier()
                
            // store the overall sum in the result-buffer
            if tid = 0 then
                result.[gid.X + groups.X * gid.Y] <- mem.[0]

        }
       
    [<LocalSize(X = 8, Y = 8)>]
    let dot2d (zero : Expr<'b>) (mul : Expr<V4d -> V4d -> 'b>) (add : Expr<'b -> 'b -> 'b>) (lLevel : int) (rLevel : int) (result : 'b[]) =
        compute {
            let mem = allocateShared<'b> 128

            let size = lSampler.GetSize(lLevel)
            let rcpSize = 1.0 / V2d size

            let lid = getLocalId().XY
            let gid = getWorkGroupId().XY
            let groups = getWorkGroupCount().XY

            // index calculations
            let id = gid * 16 + lid * 2
            let tc00 = (V2d id + V2d.Half) * rcpSize
            let tc01 = tc00 + V2d(rcpSize.X, 0.0)
            let tc10 = tc00 + V2d(0.0, rcpSize.Y)
            let tc11 = tc00 + rcpSize
            let tid = lid.X + 8 * lid.Y

            // load existing values into local memory
            let v0 =
                (%add) 
                    ((%mul) (lSampler.SampleLevel(tc00, float lLevel)) (rSampler.SampleLevel(tc00, float rLevel)))
                    ((%mul) (lSampler.SampleLevel(tc01, float lLevel)) (rSampler.SampleLevel(tc01, float rLevel)))
                
            let v1 =
                (%add) 
                    ((%mul) (lSampler.SampleLevel(tc10, float lLevel)) (rSampler.SampleLevel(tc10, float rLevel)))
                    ((%mul) (lSampler.SampleLevel(tc11, float lLevel)) (rSampler.SampleLevel(tc11, float rLevel)))

            mem.[tid * 2 + 0] <- v0
            mem.[tid * 2 + 1] <- v1
            barrier()
            
            // sum the local values from right to left
            let mutable s = 64
            while s > 0 do
                if tid < s then
                    mem.[tid] <- (%add) mem.[tid] mem.[tid + s]

                s <- s >>> 1
                barrier()
                
            // store the overall sum in the result-buffer
            if tid = 0 then
                result.[gid.X + groups.X * gid.Y] <- mem.[0]

        }
              
    [<LocalSize(X = halfFoldSize)>]
    let sum1d (zero : Expr<'b>) (add : Expr<'b -> 'b -> 'b>) (cnt : int) (arr : 'b[]) (result : 'b[]) =
        compute {
            let mem = allocateShared<'b> foldSize
            let tid = getLocalId().X
            let gid = getWorkGroupId().X
            
            // index calculations
            let lai = 2 * tid
            let lbi = lai + 1
            let ai  = foldSize * gid + lai
            let bi  = ai + 1 
            
            // load existing values into local memory
            mem.[lai] <- if ai < cnt then arr.[ai] else %zero
            mem.[lbi] <- if bi < cnt then arr.[bi] else %zero
            barrier()

            // sum the local values from right to left
            let mutable s = halfFoldSize
            while s > 0 do
                if tid < s then
                    mem.[tid] <- (%add) mem.[tid] mem.[tid + s]
                s <- s >>> 1
                barrier()

            // store the overall sum in the result-buffer
            if tid = 0 then
                result.[gid] <- mem.[0]

        }


type CgSolver<'a when 'a : unmanaged>(runtime : IRuntime, conv : Expr<V4d -> 'a>) =
    static let num = NumInstances.instance<'a>
    static let rnum = ReflectedNum.instance<'a>

    let v4Mul = <@ fun a b -> (%rnum.mul) ((%conv) a) ((%conv) b) @>
    let v4Add = <@ fun a b -> (%rnum.add) ((%conv) a) ((%conv) b) @>

    static let ceilDiv (a : int) (b : int) =
        if a % b = 0 then a / b
        else 1 + a / b
    
    static let ceilDiv2 (a : V2i) (b : V2i) =
        V2i(
            ceilDiv a.X b.X,
            ceilDiv a.Y b.Y
        )
    
    let dot1d = runtime.CreateComputeShader (CgSolverShader.dot1d rnum.zero rnum.mul rnum.add)
    let dot2d = runtime.CreateComputeShader (CgSolverShader.dot2d rnum.zero v4Mul rnum.add)
    let sum1d = runtime.CreateComputeShader (CgSolverShader.sum1d rnum.zero rnum.add)
    let sum2d = runtime.CreateComputeShader (CgSolverShader.sum2d rnum.zero v4Add rnum.add)

    member x.Sum(v : IBuffer<'a>) =
        if v.Count <= 0 then
            num.zero

        elif v.Count = 1 then
            let arr = Array.zeroCreate 1
            v.Download(arr)
            arr.[0]

        else
            let resCnt = ceilDiv v.Count CgSolverShader.foldSize
            use res = runtime.CreateBuffer<'a>(resCnt)
            use input = runtime.NewInputBinding sum1d
            input.["arr"] <- v
            input.["cnt"] <- v.Count
            input.["result"] <- res
            input.Flush()
            
            runtime.Run [
                ComputeCommand.Bind sum1d
                ComputeCommand.SetInput input
                ComputeCommand.Dispatch resCnt
                ComputeCommand.Sync(res.Buffer, ResourceAccess.ShaderWrite, ResourceAccess.ShaderRead)
            ]

            x.Sum(res)

    member x.Sum(v : ITextureSubResource) =
        let size = v.Size.XY
        
        if size.AnySmallerOrEqual 0 then
            num.zero
            
        else
            let resCnt = ceilDiv2 size (V2i(16,16))
            
            use res = runtime.CreateBuffer<'a>(resCnt.X * resCnt.Y)
            use input = runtime.NewInputBinding sum2d
            input.["l"] <- v.Texture
            input.["lLevel"] <- v.Level
            input.["result"] <- res
            input.Flush()

            runtime.Run [
                ComputeCommand.Bind sum2d
                ComputeCommand.SetInput input
                ComputeCommand.Dispatch resCnt
                ComputeCommand.Sync(res.Buffer, ResourceAccess.ShaderWrite, ResourceAccess.ShaderRead)
            ]

            x.Sum(res)

    member x.Dot(l : IBuffer<'a>, r : IBuffer<'a>) =
        if l.Count <> r.Count then failwith "buffers have mismatching size"
        let cnt = l.Count
        
        if cnt <= 0 then
            num.zero

        elif cnt = 1 then
            let la = Array.zeroCreate 1
            let ra = Array.zeroCreate 1
            l.Download(la)
            r.Download(ra)
            num.mul la.[0] ra.[0]

        else
            let resCnt = ceilDiv cnt CgSolverShader.foldSize
            
            use res = runtime.CreateBuffer<'a>(resCnt)
            use input = runtime.NewInputBinding dot1d
            input.["l"] <- l
            input.["r"] <- r
            input.["cnt"] <- l.Count
            input.["result"] <- res
            input.Flush()

            runtime.Run [
                ComputeCommand.Bind dot1d
                ComputeCommand.SetInput input
                ComputeCommand.Dispatch resCnt
                ComputeCommand.Sync(res.Buffer, ResourceAccess.ShaderWrite, ResourceAccess.ShaderRead)
            ]

            x.Sum(res)

    //member inline x.Length(l : IBuffer<'a>) =
    //    let r = x.Dot(l,l)
    //    sqrt r

    member x.Dot(l : ITextureSubResource, r : ITextureSubResource) =  
        if l.Size.XY <> r.Size.XY then failwith "buffers have mismatching size"
        let size = l.Size.XY
        
        if size.AnySmallerOrEqual 0 then
            num.zero
            
        else
            let resCnt = ceilDiv2 size (V2i(16,16))
            
            use res = runtime.CreateBuffer<'a>(resCnt.X * resCnt.Y)
            use input = runtime.NewInputBinding dot2d
            input.["l"] <- l.Texture
            input.["r"] <- r.Texture
            input.["lLevel"] <- l.Level
            input.["rLevel"] <- r.Level
            input.["result"] <- res
            input.Flush()

            runtime.Run [
                ComputeCommand.Bind dot2d
                ComputeCommand.SetInput input
                ComputeCommand.Dispatch resCnt
                ComputeCommand.Sync(res.Buffer, ResourceAccess.ShaderWrite, ResourceAccess.ShaderRead)
            ]

            x.Sum(res)

    member x.Dot(l : PixImage, r : PixImage) =
        let tl = runtime.CreateTexture (l.Size, TextureFormat.ofPixFormat l.PixFormat TextureParams.empty, 1, 1)
        let tr = runtime.CreateTexture (r.Size, TextureFormat.ofPixFormat r.PixFormat TextureParams.empty, 1, 1)
        runtime.Upload(tl, 0, 0, l)
        runtime.Upload(tr, 0, 0, r)
        
        try
            x.Dot(tl.[TextureAspect.Color, 0, 0], tr.[TextureAspect.Color, 0, 0])
        finally
            runtime.DeleteTexture tl
            runtime.DeleteTexture tr

    member x.Sum(v : PixImage) =
        let tl = runtime.CreateTexture (v.Size, TextureFormat.ofPixFormat v.PixFormat TextureParams.empty, 1, 1)
        runtime.Upload(tl, 0, 0, v)
        try x.Sum(tl.[TextureAspect.Color, 0, 0])
        finally runtime.DeleteTexture tl


let app = new HeadlessVulkanApplication()
let runtime = app.Runtime
let s = CgSolver<float32>(runtime, <@ fun v -> float32 v.X @>)

let dot (a : float32[]) (b : float32[]) =
    use a = runtime.CreateBuffer a
    use b = runtime.CreateBuffer b
    s.Dot(a,b)

let testDot () =
    use a = runtime.CreateBuffer (Array.init 128 float32)
    use b = runtime.CreateBuffer (Array.init 128 float32)
    s.Dot(a,b)

let testImageDot() = 
    let rand = RandomSystem()
    let size = V2i(10 + rand.UniformInt(300), 10 + rand.UniformInt(300))
    let img = PixImage<byte>(Col.Format.RGBA, size)
    img.GetMatrix<C4b>().SetByIndex (fun _ -> rand.UniformC3f().ToC4b()) |> ignore
    
    let gpu = s.Dot(img, img)
    let cpu =
        let r = img.GetChannel(Col.Channel.Red)
        r.InnerProduct(r, (fun l r -> (float32 l / 255.0f) * (float32 r / 255.0f)), 0.0f, (+))
    printfn "size: %A" size
    printfn "gpu:  %.5e" gpu
    printfn "cpu:  %.5e" cpu

    
let testImageSum() = 
    let rand = RandomSystem()
    let size = V2i(10 + rand.UniformInt(300), 10 + rand.UniformInt(300))
    let img = PixImage<byte>(Col.Format.RGBA, size)
    img.GetMatrix<C4b>().SetByIndex (fun _ -> rand.UniformC3f().ToC4b()) |> ignore
    
    let gpu = s.Sum(img)
    let cpu =
        let r = img.GetChannel(Col.Channel.Red)
        r.InnerProduct(r, (fun l r -> (float32 l / 255.0f)), 0.0f, (+))
    printfn "size: %A" size
    printfn "gpu:  %.5e" gpu
    printfn "cpu:  %.5e" cpu
