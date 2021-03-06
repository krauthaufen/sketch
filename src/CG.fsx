#load "load.fsx"

open Aardvark.Base
open System.Runtime.CompilerServices
open Aardvark.Base.Rendering
open Microsoft.FSharp.Quotations
open Aardvark.Rendering.Vulkan
open System.Runtime.InteropServices

do
    let dir = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "bin")
    let info = System.IO.DirectoryInfo dir
    if not info.Exists then info.Create()
    System.AppDomain.CurrentDomain.SetData("APPBASE", dir)
    System.Environment.CurrentDirectory <- dir
    Ag.initialize()
    Aardvark.Init()

type Real<'a> =
    {
        zero : 'a
        one : 'a
        add : 'a -> 'a -> 'a
        sub : 'a -> 'a -> 'a
        mul : 'a -> 'a -> 'a
        div : 'a -> 'a -> 'a
        neg : 'a -> 'a
        pow : 'a -> int -> 'a
        sqrt : 'a -> 'a
        fromInt : int -> 'a
        fromFloat : float -> 'a
        isTiny : 'a -> bool
        isTinyEps : float -> 'a -> bool
        isPositive : 'a -> bool
    }

module RealInstances =

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
            sqrt = sqrt
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
            sqrt = sqrt
            fromInt = float
            fromFloat = float
            isTiny = Fun.IsTiny 
            isTinyEps = fun e v -> Fun.IsTiny(v, e)
            isPositive = fun v -> v > 0.0
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
            sqrt = fun v -> V2f(sqrt v.X, sqrt v.Y)
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
            sqrt = fun v -> V2d(sqrt v.X, sqrt v.Y)
            fromInt = fun v -> V2d(float v, float v)
            fromFloat = fun v -> V2d(v, v)
            isTiny = fun v -> Fun.IsTiny(v.X) && Fun.IsTiny(v.Y)
            isTinyEps = fun e v -> Fun.IsTiny(v.X, e) && Fun.IsTiny(v.Y, e)
            isPositive = fun v -> v.AllGreater 0.0
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
            sqrt = fun v -> V3f(sqrt v.X, sqrt v.Y, sqrt v.Z)
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
            sqrt = fun v -> V3d(sqrt v.X, sqrt v.Y, sqrt v.Z)
            fromInt = fun v -> V3d(float v, float v, float v)
            fromFloat = fun v -> V3d(v, v, v)
            isTiny = fun v -> Fun.IsTiny(v.X) && Fun.IsTiny(v.Y) && Fun.IsTiny(v.Z)
            isTinyEps = fun e v -> Fun.IsTiny(v.X, e) && Fun.IsTiny(v.Y, e) && Fun.IsTiny(v.Z, e)
            isPositive = fun v -> v.AllGreater 0.0
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
            sqrt = fun v -> V4f(sqrt v.X, sqrt v.Y, sqrt v.Z, sqrt v.W)
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
            sqrt = fun v -> V4d(sqrt v.X, sqrt v.Y, sqrt v.Z, sqrt v.W)
            fromInt = fun v -> V4d(float v, float v, float v, float v)
            fromFloat = fun v -> V4d(v, v, v, v)
            isTiny = fun v -> Fun.IsTiny v.X && Fun.IsTiny v.Y && Fun.IsTiny v.Z && Fun.IsTiny v.W
            isTinyEps = fun e v -> Fun.IsTiny(v.X, e) && Fun.IsTiny(v.Y, e) && Fun.IsTiny(v.Z, e) && Fun.IsTiny(v.W, e)
            isPositive = fun v -> v.AllGreater 0.0
        }

    let internal table =
        LookupTable.lookupTable [
            typeof<float32>,    Cfloat32 :> obj
            typeof<float>,      Cfloat64 :> obj
            typeof<V2f>,        CV2f :> obj
            typeof<V2d>,        CV2d :> obj
            typeof<V3f>,        CV3f :> obj
            typeof<V3d>,        CV3d :> obj
            typeof<V4f>,        CV4f :> obj
            typeof<V4d>,        CV4d :> obj
        ]

    let instance<'a> = table typeof<'a> |> unbox<Real<'a>>

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


    let cross<'p, 'c when 'p : comparison> (num : Real<'c>) (l : MapExt<MapExt<string * 'p, int>, 'c>) (r : MapExt<MapExt<string * 'p, int>, 'c>) : MapExt<MapExt<string * 'p, int>, 'c> =
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

    static let num = RealInstances.instance<'c>

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
        static let num = RealInstances.instance<'a>

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
        let num = RealInstances.instance<'c>
        fun i -> Polynomial<'p, 'c> ( MapExt.ofList [ MapExt.ofList [(name, i), 1], num.one ] )

module Polynomial =

    [<GeneralizableValue>]
    let zero<'p, 'c when 'p : comparison> = Polynomial<'p, 'c>.Zero

    [<GeneralizableValue>]
    let one<'p, 'c when 'p : comparison> = Polynomial<'p, 'c>.One

    let inline evaluate (values : MapExt<string * 'p, 'c>) (p : Polynomial<'p, 'c>) = p.Evaluate(values)


    type Data<'c, 'a, 'v> =
        {
            num     : Real<'v>
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

                            //<v|A*x>

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
                num     = RealInstances.instance<'v>
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
                num     = RealInstances.instance<'v>
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
                num     = RealInstances.instance<'v>
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
                num     = RealInstances.instance<'v>
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



type RReal<'a> =
    {
        zero    : Expr<'a>
        one     : Expr<'a>
        add     : Expr<'a -> 'a -> 'a>
        sub     : Expr<'a -> 'a -> 'a>
        neg     : Expr<'a -> 'a>
        mul     : Expr<'a -> 'a -> 'a>
        div     : Expr<'a -> 'a -> 'a>
        pow     : Expr<'a -> int -> 'a>

        min     : Expr<'a -> 'a -> 'a>
        max     : Expr<'a -> 'a -> 'a>
        pinf    : Expr<'a>
        ninf    : Expr<'a>

        fromV4  : Expr<V4d -> 'a>
        format  : TextureFormat
    }

module ReflectedReal =
    open FShade 

    [<GLSLIntrinsic("(1.0 / 0.0)")>]
    let pinf() : float = onlyInShaderCode "pinf" //System.Double.PositiveInfinity
    
    [<GLSLIntrinsic("(-1.0 / 0.0)")>]
    let ninf() : float = onlyInShaderCode "ninf" //System.Double.NegativeInfinity
    
    [<GLSLIntrinsic("(1.0 / 0.0)")>]
    let fpinf() : float32 = onlyInShaderCode "pinf" //System.Single.PositiveInfinity
    
    [<GLSLIntrinsic("(-1.0 / 0.0)")>]
    let fninf() : float32 = onlyInShaderCode "ninf" //System.Single.NegativeInfinity

    let Cfloat64 =
        {
            zero    = <@ 0.0 @>
            one     = <@ 1.0 @>
            add     = <@ (+) @>
            sub     = <@ (-) @>
            mul     = <@ (*) @>
            div     = <@ (/) @>
            neg     = <@ (~-) @>
            pow     = <@ pown @>

            min     = <@ min @>
            max     = <@ max @>
            pinf    = <@ pinf() @>
            ninf    = <@ ninf() @>

            fromV4  = <@ fun v -> v.X @>
            format  = TextureFormat.R32f
        }

    let Cfloat32 =
        {
            zero    = <@ 0.0f @>
            one     = <@ 1.0f @>
            add     = <@ (+) @>
            sub     = <@ (-) @>
            mul     = <@ (*) @>
            div     = <@ (/) @>
            neg     = <@ (~-) @>
            pow     = <@ pown @>

            min     = <@ min @>
            max     = <@ max @>
            pinf    = <@ fpinf() @>
            ninf    = <@ fninf() @>
            fromV4  = <@ fun v -> float32 v.X @>
            format  = TextureFormat.R32f
        }

    let CV2f =
        {
            zero    = <@ V2f.Zero @>
            one     = <@ V2f.II @>
            add     = <@ (+) @>
            sub     = <@ (-) @>
            mul     = <@ (*) @>
            div     = <@ (/) @>
            neg     = <@ (~-) @>
            pow     = <@ fun v e -> V2f(pown v.X e, pown v.Y e) @>
            min     = <@ fun l r -> V2f(min l.X r.X, min l.Y r.Y) @>
            max     = <@ fun l r -> V2f(max l.X r.X, max l.Y r.Y) @>
            pinf    = <@ V2f(fpinf(), fpinf()) @>
            ninf    = <@ V2f(fninf(), fninf()) @>
            fromV4  = <@ fun v -> V2f v.XY @>
            format  = TextureFormat.Rg32f
        }

    let CV2d =
        {
            zero    = <@ V2d.Zero @>
            one     = <@ V2d.II @>
            add     = <@ (+) @>
            sub     = <@ (-) @>
            mul     = <@ (*) @>
            div     = <@ (/) @>
            neg     = <@ (~-) @>
            pow     = <@ fun v e -> V2d(pown v.X e, pown v.Y e) @>
            min     = <@ fun l r -> V2d(min l.X r.X, min l.Y r.Y) @>
            max     = <@ fun l r -> V2d(max l.X r.X, max l.Y r.Y) @>
            pinf    = <@ V2d(pinf(), pinf()) @>
            ninf    = <@ V2d(ninf(), ninf()) @>
            fromV4  = <@ fun v -> v.XY @>
            format  = TextureFormat.Rg32f
        }

    let CV3f =
        {
            zero    = <@ V3f.Zero @>
            one     = <@ V3f.III @>
            add     = <@ (+) @>
            sub     = <@ (-) @>
            mul     = <@ (*) @>
            div     = <@ (/) @>
            neg     = <@ (~-) @>
            pow     = <@ fun v e -> V3f(pown v.X e, pown v.Y e, pown v.Z e) @>
            min     = <@ fun l r -> V3f(min l.X r.X, min l.Y r.Y, min l.Z r.Z) @>
            max     = <@ fun l r -> V3f(max l.X r.X, max l.Y r.Y, max l.Z r.Z) @>
            pinf    = <@ V3f(fpinf(), fpinf(), fpinf()) @>
            ninf    = <@ V3f(fninf(), fninf(), fninf()) @>
            fromV4  = <@ fun v -> V3f v.XYZ @>
            format  = TextureFormat.Rgb32f
        }

    let CV3d =
        {
            zero    = <@ V3d.Zero @>
            one     = <@ V3d.III @>
            add     = <@ (+) @>
            sub     = <@ (-) @>
            mul     = <@ (*) @>
            div     = <@ (/) @>
            neg     = <@ (~-) @>
            pow     = <@ fun v e -> V3d(pown v.X e, pown v.Y e, pown v.Z e) @>
            min     = <@ fun l r -> V3d(min l.X r.X, min l.Y r.Y, min l.Z r.Z) @>
            max     = <@ fun l r -> V3d(max l.X r.X, max l.Y r.Y, max l.Z r.Z) @>
            pinf    = <@ V3d(pinf(), pinf(), pinf()) @>
            ninf    = <@ V3d(ninf(), ninf(), ninf()) @>
            fromV4  = <@ fun v -> v.XYZ @>
            format  = TextureFormat.Rgb32f
        }

    let CV4f =
        {
            zero    = <@ V4f.Zero @>
            one     = <@ V4f.IIII @>
            add     = <@ (+) @>
            sub     = <@ (-) @>
            mul     = <@ (*) @>
            div     = <@ (/) @>
            neg     = <@ (~-) @>
            pow     = <@ fun v e -> V4f(pown v.X e, pown v.Y e, pown v.Z e, pown v.W e) @>
            min     = <@ fun l r -> V4f(min l.X r.X, min l.Y r.Y, min l.Z r.Z, min l.W r.W) @>
            max     = <@ fun l r -> V4f(max l.X r.X, max l.Y r.Y, max l.Z r.Z, max l.W r.W) @>
            pinf    = <@ V4f(fpinf(), fpinf(), fpinf(), fpinf()) @>
            ninf    = <@ V4f(fninf(), fninf(), fninf(), fninf()) @>
            fromV4  = <@ fun v -> V4f v @>
            format  = TextureFormat.Rgba32f
        }

    let CV4d =
        {
            zero    = <@ V4d.Zero @>
            one     = <@ V4d.IIII @>
            add     = <@ (+) @>
            sub     = <@ (-) @>
            mul     = <@ (*) @>
            div     = <@ (/) @>
            neg     = <@ (~-) @>
            pow     = <@ fun v e -> V4d(pown v.X e, pown v.Y e, pown v.Z e, pown v.W e) @>
            min     = <@ fun l r -> V4d(min l.X r.X, min l.Y r.Y, min l.Z r.Z, min l.W r.W) @>
            max     = <@ fun l r -> V4d(max l.X r.X, max l.Y r.Y, max l.Z r.Z, max l.W r.W) @>
            pinf    = <@ V4d(pinf(), pinf(), pinf(), pinf()) @>
            ninf    = <@ V4d(ninf(), ninf(), ninf(), ninf()) @>
            fromV4  = <@ fun v -> v @>
            format  = TextureFormat.Rgba32f
        }

    let internal table =
        LookupTable.lookupTable [
            typeof<float32>,    Cfloat32 :> obj
            typeof<float>,      Cfloat64 :> obj
            typeof<V2f>,        CV2f :> obj
            typeof<V2d>,        CV2d :> obj
            typeof<V3f>,        CV3f :> obj
            typeof<V3d>,        CV3d :> obj
            typeof<V4f>,        CV4f :> obj
            typeof<V4d>,        CV4d :> obj
        ]

    let instance<'a> = table typeof<'a> |> unbox<RReal<'a>>

module TensorToolShaders =
    open FShade

    [<Literal>]
    let foldSize = 128
    
    [<Literal>]
    let halfFoldSize = 64

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


    [<LocalSize(X = halfFoldSize)>]
    let fold1d (zero : Expr<'b>) (add : Expr<'b -> 'b -> 'b>) (cnt : int) (arr : 'b[]) (result : 'b[]) =
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

    [<LocalSize(X = 8, Y = 8)>]
    let fold2d (zero : Expr<'b>) (addV4 : Expr<V4d -> V4d -> 'b>) (add : Expr<'b -> 'b -> 'b>) (lLevel : int) (result : 'b[]) =
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
              

    [<LocalSize(X = 64)>]
    let mad1d (mul : Expr<'a -> 'b -> 'c>) (add : Expr<'d -> 'c -> 'd>) (cnt : int) (src : 'a[]) (factor : 'b) (dst : 'd[]) =
        compute {
            let id = getGlobalId().X
            if id < cnt then
                dst.[id] <- (%add) dst.[id] ((%mul) src.[id] factor)
        }


type TensorTools<'a when 'a : unmanaged>(runtime : IRuntime) =
    static let num = RealInstances.instance<'a>
    static let rnum = ReflectedReal.instance<'a>




    let conv = rnum.fromV4
    let v4Mul = <@ fun a b -> (%rnum.mul) ((%conv) a) ((%conv) b) @>
    let v4Add = <@ fun a b -> (%rnum.add) ((%conv) a) ((%conv) b) @>
    let v4Max = <@ fun a b -> (%rnum.max) ((%conv) a) ((%conv) b) @>
    let v4Min = <@ fun a b -> (%rnum.min) ((%conv) a) ((%conv) b) @>

    static let ceilDiv (a : int) (b : int) =
        if a % b = 0 then a / b
        else 1 + a / b
    
    static let ceilDiv2 (a : V2i) (b : V2i) =
        V2i(
            ceilDiv a.X b.X,
            ceilDiv a.Y b.Y
        )

    let withImage (img : PixImage) (action : IBackendTexture -> 'r) =
        let tex = runtime.CreateTexture (img.Size, TextureFormat.ofPixFormat img.PixFormat TextureParams.empty, 1, 1)
        runtime.Upload(tex, 0, 0, img)
        try action tex
        finally runtime.DeleteTexture tex

    let withBuffer (data : 'a[]) (action : IBuffer<'a> -> 'r) =
        use b = runtime.CreateBuffer data
        action b
        
    let withBuffer2d (data : 'a[,]) (action : IBuffer<'a> -> 'r) =
        let cnt = data.GetLength(0) * data.GetLength(1)
        use buffer = runtime.CreateBuffer<'a>(cnt)
        let gc = GCHandle.Alloc(data, GCHandleType.Pinned) 
        try buffer.Upload(gc.AddrOfPinnedObject(), nativeint sizeof<'a> * nativeint cnt)
        finally gc.Free()
        action buffer

    let dot1d = runtime.CreateComputeShader (TensorToolShaders.dot1d rnum.zero rnum.mul rnum.add)
    let dot2d = runtime.CreateComputeShader (TensorToolShaders.dot2d rnum.zero v4Mul rnum.add)
    let sum1d = runtime.CreateComputeShader (TensorToolShaders.fold1d rnum.zero rnum.add)
    let sum2d = runtime.CreateComputeShader (TensorToolShaders.fold2d rnum.zero v4Add rnum.add)

    let mul1d = lazy ( runtime.CreateComputeShader (TensorToolShaders.fold1d rnum.one rnum.mul) )
    let max1d = lazy ( runtime.CreateComputeShader (TensorToolShaders.fold1d rnum.ninf rnum.max) )
    let min1d = lazy ( runtime.CreateComputeShader (TensorToolShaders.fold1d rnum.pinf rnum.min) )
    
    let mul2d = lazy ( runtime.CreateComputeShader (TensorToolShaders.fold2d rnum.one v4Mul rnum.mul) )
    let max2d = lazy ( runtime.CreateComputeShader (TensorToolShaders.fold2d rnum.ninf v4Max rnum.max) )
    let min2d = lazy ( runtime.CreateComputeShader (TensorToolShaders.fold2d rnum.pinf v4Min rnum.min) )
    
    let mad1d = runtime.CreateComputeShader (TensorToolShaders.mad1d rnum.mul rnum.add)

    let rec fold1d (zero : 'a) (shader : IComputeShader) (v : IBuffer<'a>) =
        if v.Count <= 0 then
            zero

        elif v.Count = 1 then
            let arr = Array.zeroCreate 1
            v.Download(arr)
            arr.[0]

        else
            let resCnt = ceilDiv v.Count TensorToolShaders.foldSize
            use res = runtime.CreateBuffer<'a>(resCnt)
            use input = runtime.NewInputBinding shader
            input.["arr"] <- v
            input.["cnt"] <- v.Count
            input.["result"] <- res
            input.Flush()
            
            runtime.Run [
                ComputeCommand.Bind shader
                ComputeCommand.SetInput input
                ComputeCommand.Dispatch resCnt
                ComputeCommand.Sync(res.Buffer, ResourceAccess.ShaderWrite, ResourceAccess.ShaderRead)
            ]

            fold1d zero shader res

    let rec fold2d (zero : 'a) (shader : IComputeShader) (shader1d : IComputeShader) (v : ITextureSubResource) =
        let size = v.Size.XY
        
        if size.AnySmallerOrEqual 0 then
            zero
            
        else
            let resCnt = ceilDiv2 size (V2i(16,16))
            
            use res = runtime.CreateBuffer<'a>(resCnt.X * resCnt.Y)
            use input = runtime.NewInputBinding shader
            input.["l"] <- v.Texture
            input.["lLevel"] <- v.Level
            input.["result"] <- res
            input.Flush()

            runtime.Run [
                ComputeCommand.Bind shader
                ComputeCommand.SetInput input
                ComputeCommand.Dispatch resCnt
                ComputeCommand.Sync(res.Buffer, ResourceAccess.ShaderWrite, ResourceAccess.ShaderRead)
            ]
            
            fold1d zero shader1d res
            
    member x.Sum(v : IBuffer<'a>) = fold1d num.zero sum1d v
    member x.Product(v : IBuffer<'a>) = fold1d num.one mul1d.Value v
    member x.Min(v : IBuffer<'a>) = fold1d (num.div num.one num.zero) min1d.Value v
    member x.Max(v : IBuffer<'a>) = fold1d (num.div (num.neg num.one) num.zero) max1d.Value v
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
            let resCnt = ceilDiv cnt TensorToolShaders.foldSize
            
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

    member x.Length(l : IBuffer<'a>) =
        let r = x.Dot(l,l)
        num.sqrt r
        
    member x.LengthSquared(l : IBuffer<'a>) =
        x.Dot(l,l)

    member x.Average(v : IBuffer<'a>) =
        num.div (x.Sum v) (num.fromInt v.Count)
        
    member x.Variance(v : IBuffer<'a>) =
        let e0 = num.div (x.LengthSquared v) (num.fromInt v.Count)
        let e1 = num.pow (x.Average v) 2
        num.sub e0 e1

    member x.MultiplyAdd(src : IBuffer<'a>, f : 'a, dst : IBuffer<'a>) =
        let cnt = min src.Count dst.Count
        if cnt > 0 then
            use input = runtime.NewInputBinding mad1d
            input.["src"] <- src
            input.["cnt"] <- cnt
            input.["dst"] <- dst
            input.["factor"] <- f
            input.Flush()

            runtime.Run [
                ComputeCommand.Bind mad1d
                ComputeCommand.SetInput input
                ComputeCommand.Dispatch (ceilDiv cnt mad1d.LocalSize.X)
            ]


    member x.Sum(v : ITextureSubResource) = fold2d num.zero sum2d sum1d v
    member x.Product(v : ITextureSubResource) = fold2d num.one mul2d.Value mul1d.Value v
    member x.Min(v : ITextureSubResource) = fold2d (num.div num.one num.zero) min2d.Value min1d.Value v
    member x.Max(v : ITextureSubResource) = fold2d (num.div (num.neg num.one) num.zero) max2d.Value max1d.Value v
        
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

    member x.Length(l : ITextureSubResource) =
        let r = x.Dot(l,l)
        num.sqrt r
        
    member x.LengthSquared(l : ITextureSubResource) =
        x.Dot(l,l)
        
    member x.Average(v : ITextureSubResource) =
        num.div (x.Sum v) (num.fromInt (v.Size.X * v.Size.Y))
        
    member x.Variance(v : ITextureSubResource) =
        let e0 = num.div (x.LengthSquared v) (num.fromInt (v.Size.X * v.Size.Y))
        let e1 = num.pow (x.Average v) 2
        num.sub e0 e1


    member x.Sum(v : 'a[]) = withBuffer v x.Sum
    member x.Product(v : 'a[]) = withBuffer v x.Product
    member x.Min(v : 'a[]) = withBuffer v x.Min
    member x.Max(v : 'a[]) = withBuffer v x.Max
    member x.Dot(l : 'a[], r : 'a[]) = withBuffer l (fun lb -> withBuffer r (fun rb -> x.Dot(lb, rb)))
    member x.Length(v : 'a[]) = withBuffer v x.Length
    member x.LengthSquared(v : 'a[]) = withBuffer v x.LengthSquared
    member x.Average(v : 'a[]) = withBuffer v x.Average
    member x.Variance(v : 'a[]) = withBuffer v x.Variance
    member x.MultiplyAdd(src : 'a[], f : 'a, dst : 'a[]) = withBuffer src (fun src -> withBuffer dst (fun dst -> x.MultiplyAdd(src, f, dst); dst.Download()))


    member x.Sum(v : 'a[,]) = withBuffer2d v x.Sum
    member x.Product(v : 'a[,]) = withBuffer2d v x.Product
    member x.Min(v : 'a[,]) = withBuffer2d v x.Min
    member x.Max(v : 'a[,]) = withBuffer2d v x.Max
    member x.Dot(l : 'a[,], r : 'a[,]) = withBuffer2d l (fun lb -> withBuffer2d r (fun rb -> x.Dot(lb, rb)))
    member x.Length(v : 'a[,]) = withBuffer2d v x.Length
    member x.LengthSquared(v : 'a[,]) = withBuffer2d v x.LengthSquared
    member x.Average(v : 'a[,]) = withBuffer2d v x.Average
    member x.Variance(v : 'a[,]) = withBuffer2d v x.Variance
    member x.MultiplyAdd(src : 'a[,], f : 'a, dst : 'a[,]) = 
        withBuffer2d src (fun bsrc -> 
            withBuffer2d dst (fun bdst -> 
                x.MultiplyAdd(bsrc, f, bdst)
                let cnt = (src.GetLength 0) * (src.GetLength 1)
                let res : 'a[,] = Array2D.zeroCreate (src.GetLength 0) (src.GetLength 1)
                let gc = GCHandle.Alloc(res, GCHandleType.Pinned)
                try
                    bdst.Buffer.Download(0n, gc.AddrOfPinnedObject(), nativeint sizeof<'a> * nativeint cnt)
                    res
                finally
                    gc.Free()
            )
        )



    member x.Sum(v : PixImage) = withImage v (fun t -> x.Sum(t.[TextureAspect.Color, 0, 0]))
    member x.Product(v : PixImage) = withImage v (fun t -> x.Product(t.[TextureAspect.Color, 0, 0]))
    member x.Min(v : PixImage) = withImage v (fun t -> x.Min(t.[TextureAspect.Color, 0, 0]))
    member x.Max(v : PixImage) = withImage v (fun t -> x.Max(t.[TextureAspect.Color, 0, 0]))
    member x.Dot(l : PixImage, r : PixImage) = withImage l (fun tl -> withImage r (fun tr -> x.Dot(tl.[TextureAspect.Color, 0, 0], tr.[TextureAspect.Color, 0, 0])))
    member x.Length(v : PixImage) = withImage v (fun t -> x.Length(t.[TextureAspect.Color, 0, 0]))
    member x.LengthSquared(v : PixImage) = withImage v (fun t -> x.LengthSquared(t.[TextureAspect.Color, 0, 0]))
    member x.Average(v : PixImage) = withImage v (fun t -> x.Average(t.[TextureAspect.Color, 0, 0]))
    member x.Variance(v : PixImage) = withImage v (fun t -> x.Variance(t.[TextureAspect.Color, 0, 0]))
        

    //member x.EvalPolynomial(p : Polynomial<int, 'a>, buffer : IBuffer<'a>, dst : IBuffer<'a>) =
    //    let offsets, factors, exponents = 
    //        MapExt.toArray p.coefficients
    //            |> Array.collect (fun (k,f) ->
    //                if MapExt.isEmpty k then
    //                    [| (0, f, 0) |]
    //                else
    //                    k |> MapExt.toArray |> Array.map (fun ((_,o), e) ->
    //                        (o, f, e)
    //                    )
    //            )
    //            |> Array.unzip3

    //    use bo = runtime.CreateBuffer offsets
    //    use bf = runtime.CreateBuffer factors
    //    use be = runtime.CreateBuffer exponents

    //    use input = runtime.NewInputBinding poly1d
    //    input.["offsets"] <- bo
    //    input.["factors"] <- bf
    //    input.["exponents"] <- be
    //    input.["cnt"] <- buffer.Count
    //    input.["x"] <- buffer
    //    input.["res"] <- dst
    //    input.["PolynomialCount"] <- offsets.Length
    //    input.Flush()

    //    runtime.Run [
    //        ComputeCommand.Bind poly1d
    //        ComputeCommand.SetInput input
    //        ComputeCommand.Dispatch (ceilDiv buffer.Count poly1d.LocalSize.X)
    //    ]


module CGShaders =
    open FShade
    open FShade.Imperative
        


    let toExpr1d (getBuffer : string -> Expr<'c[]> * Expr<int>) (offset : Expr<int>) (p : Polynomial<int, 'c>) =
        let real = ReflectedReal.instance<'c>
        let mutable bindings = MapExt.empty
        let rm = real.mul
        let ra = real.add

        let mutable cache = MapExt.empty

        let getBuffer name =
            let mutable res = Unchecked.defaultof<_>
            cache <- cache |> MapExt.alter name (function Some o -> res <- o; Some o | None -> res <- getBuffer name;  Some res)
            res


        let fetch (name : string) (idx : Expr<int>) =
            let buffer, count = getBuffer name
            <@@
                (%buffer).[clamp 0 ((%count) - 1) (%idx)]
            @@>

        let rec get (v : string) (c : int) (e : int) =
            if e = 0 then
                real.one :> Expr
            else 
                let name = 
                    if c < 0 then sprintf "%s_n%03d_%02d" v -c e
                    else sprintf "%s_p%03d_%02d" v c e
                
                match MapExt.tryFind name bindings with
                    | Some (v,e) ->
                        Expr.Var v
                    | None ->
                        let buffer, cnt = getBuffer v
                        let value = fetch v <@ (%offset) + c @>
                        let var = Var(name, typeof<'c>)
                        
                        if e = 1 then
                            bindings <- MapExt.add name (var,value) bindings
                            Expr.Var var
                        else
                            let parent = get v c (e - 1)
                            let value = <@@ (%rm) (%%parent) (%%value) @@>
                            bindings <- MapExt.add name (var, value) bindings
                            Expr.Var var

        let mul (m : MapExt<string * int, int>) =
            let m = m |> MapExt.toList
            let rec fold (m : list<(string * int) * int>) =
                match m with
                    | [] -> 
                        real.one :> Expr

                    | [((v,i),e)] ->
                        get v i e  

                    | ((v,i),e) :: rest ->
                        let a = get v i e
                        let b = fold rest
                        <@@ (%rm) (%%a) (%%b) @@>

            fold m
            
        let rec build (parts : list<MapExt<string * int, int> * 'c>) =
            match parts with
                | [] -> 
                    real.zero :> Expr
                | [m, f] ->
                    let c = mul m
                    <@@ (%rm) (%%c) f @@>
                | (m,f) :: rest ->
                    let a = mul m
                    let b = build rest
                    <@@ (%ra) ((%rm) (%%a) f) (%%b) @@>
                    
            



        let res = build (MapExt.toList p.coefficients)

        let rec warp (bindings : list<Var * Expr>) (body : Expr) =
            match bindings with
                | [] -> body
                | (v,e) :: rest ->
                    Expr.Let(v, e, warp rest body)


        warp (List.map snd (MapExt.toList bindings)) res

    let toExpr2d (getBuffer : string -> Expr<Sampler2d>) (offset : Expr<V2i>) (p : Polynomial<int * int, 'c>) =
        let real = ReflectedReal.instance<'c>
        let mutable bindings = MapExt.empty
        let rm = real.mul
        let ra = real.add
        let ofV4 = real.fromV4

        let mutable cache = MapExt.empty
        
        let sizeVar = Var("size", typeof<V2i>)
        let size : Expr<V2i> = Expr.Var sizeVar |> Expr.Cast
        let getBuffer name =
            let mutable res = Unchecked.defaultof<_>
            cache <- cache |> MapExt.alter name (function Some o -> res <- o; Some o | None -> res <- getBuffer name;  Some res)

            if not (bindings.ContainsKey "000size") then
                let tex = res
                bindings <- MapExt.add "000size" (sizeVar, <@@ (%tex).Size @@>) bindings


            res
            

        let fetch (name : string) (idx : Expr<V2i>) =
            let buffer = getBuffer name
            <@@
                (%ofV4) ((%buffer).SampleLevel((V2d (%idx) + V2d.Half) / V2d (%size), 0.0))
            @@>

        let rec get (v : string) (cx : int, cy : int) (e : int) =
            if e = 0 then
                real.one :> Expr
            else 
                let nx = if cx < 0 then sprintf "n%03d" -cx else sprintf "p%03d" cx
                let ny = if cy < 0 then sprintf "n%03d" -cy else sprintf "p%03d" cy
                let name = sprintf "%s_%s_%s_%02d" v nx ny e
                
                match MapExt.tryFind name bindings with
                    | Some (v,e) ->
                        Expr.Var v
                    | None ->
                        let buffer = getBuffer v
                        let value = fetch v <@ (%offset) + V2i(cx,cy) @>
                        let var = Var(name, typeof<'c>)
                        
                        if e = 1 then
                            bindings <- MapExt.add name (var,value) bindings
                            Expr.Var var
                        else
                            let parent = get v (cx, cy) (e - 1)
                            let value = <@@ (%rm) (%%parent) (%%value) @@>
                            bindings <- MapExt.add name (var, value) bindings
                            Expr.Var var

        let mul (m : MapExt<string * (int * int), int>) =
            let m = m |> MapExt.toList
            let rec fold (m : list<(string * (int * int)) * int>) =
                match m with
                    | [] -> 
                        real.one :> Expr

                    | [((v,i),e)] ->
                        get v i e  

                    | ((v,i),e) :: rest ->
                        let a = get v i e
                        let b = fold rest
                        <@@ (%rm) (%%a) (%%b) @@>

            fold m
            
        let rec build (parts : list<MapExt<string * (int * int), int> * 'c>) =
            match parts with
                | [] -> 
                    real.zero :> Expr
                | [m, f] ->
                    let c = mul m
                    <@@ (%rm) (%%c) f @@>
                | (m,f) :: rest ->
                    let a = mul m
                    let b = build rest
                    <@@ (%ra) ((%rm) (%%a) f) (%%b) @@>
                    
            



        let res = build (MapExt.toList p.coefficients)

        let rec warp (bindings : list<Var * Expr>) (body : Expr) =
            match bindings with
                | [] -> body
                | (v,e) :: rest ->
                    Expr.Let(v, e, warp rest body)


        warp (List.map snd (MapExt.toList bindings)) res

    type Dummy<'c> = { [<Position>] p : 'c }
    let toCCode1d (getBuffer : string -> Expr<'c[]> * Expr<int>) (offset : Expr<int>) (p : Polynomial<int, 'c>) =

        let e = toExpr1d getBuffer offset p
        let effect = 
            FShade.Effect.ofFunction (fun (v : Effects.Vertex) ->
                vertex {
                    let a = (%%e : 'c)
                    return { p = a }
                }
            )
            
        let module_ = Effect.toModule { EffectConfig.empty with EffectConfig.lastStage = ShaderStage.Vertex; EffectConfig.outputs = Map.ofList ["Positions", (typeof<'c>, 0)] } effect
        let glsl = 
            ModuleCompiler.compileGLSL430 module_

        let (_,err) = GLSLang.GLSLang.tryCompile GLSLang.ShaderStage.Vertex "main" ["Vertex"] glsl.code

        printfn "%s" err
        glsl.code
        
    let toCCode2d (getBuffer : string -> Expr<Sampler2d>) (offset : Expr<V2i>) (p : Polynomial<int * int, 'c>) =
        
        let e = toExpr2d getBuffer offset p
        let effect = 
            FShade.Effect.ofFunction (fun (v : Effects.Vertex) ->
                vertex {
                    let a = (%%e : 'c)
                    return { p = a }
                }
            )
            
        let module_ = Effect.toModule { EffectConfig.empty with EffectConfig.lastStage = ShaderStage.Vertex; EffectConfig.outputs = Map.ofList ["Positions", (typeof<'c>, 0)] } effect
        let glsl = 
            ModuleCompiler.compileGLSL430 module_

        let (_,err) = GLSLang.GLSLang.tryCompile GLSLang.ShaderStage.Vertex "main" ["Vertex"] glsl.code

        printfn "%s" err
        glsl.code

    [<GLSLIntrinsic("gl_VertexIndex")>]
    let id() : int = onlyInShaderCode "id"

    let simple1d (p : Polynomial<int, 'c>) =
        toCCode1d (fun n -> <@ uniform?StorageBuffer?x @>, <@ uniform?XCount @>) (Expr.ReadInput<int>(ParameterKind.Input, "id")) p 
        
    let simple2d (p : Polynomial<int * int, 'c>) =
        let sampler (name : string) =
            Expr.ReadInput<Sampler2d>(ParameterKind.Uniform, name)


        toCCode2d sampler (Expr.ReadInput<V2i>(ParameterKind.Input, "id")) p 





let app = new HeadlessVulkanApplication()
let runtime = app.Runtime
let s = TensorTools<float32>(runtime)

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
