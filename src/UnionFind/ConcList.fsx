open System.Collections
open System.Collections.Generic

[<StructuredFormatDisplay("{AsString}")>]
type ConcList<'a> =
    private 
        | Nil
        | Single of 'a
        | Concat of ConcList<'a> * ConcList<'a>

    member private x.AsString =
        let str = x |> Seq.toList |> List.map (sprintf "%A") |> String.concat "; "
        sprintf "conc [%s]" str

    interface IEnumerable with
        member x.GetEnumerator() = new ConcListEnumerator<'a>(x) :> IEnumerator
            
    interface IEnumerable<'a> with
        member x.GetEnumerator() = new ConcListEnumerator<'a>(x) :> IEnumerator<'a>

and private ConcListEnumerator<'a>(l : ConcList<'a>) =
    let mutable current = Unchecked.defaultof<'a>
    let mutable stack = [l]

    member x.MoveNext() =
        match stack with
            | [] -> 
                false
            | Nil :: rest ->
                stack <- rest
                x.MoveNext()
            | Single(v) :: rest ->
                current <- v
                stack <- rest
                true
            | Concat(l,r) :: rest ->
                stack <- l :: r :: rest
                x.MoveNext()

    member x.Current =
        current

    member x.Dispose() = 
        stack <- []
        current <- Unchecked.defaultof<'a>

    member x.Reset() =
        stack <- [l]
        current <- Unchecked.defaultof<'a>

    interface IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Reset() = x.Reset()
        member x.Current = x.Current :> obj

    interface IEnumerator<'a> with
        member x.Current = x.Current
        member x.Dispose() = x.Dispose()
      
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ConcList =

    type private Empty<'a> private() =
        static let instance : ConcList<'a> = Nil
        static member Instance = instance

    [<GeneralizableValue>]
    let empty<'a> : ConcList<'a> = Empty<'a>.Instance

    let singleton (v : 'a) = Single v

    let ofSeq (s : seq<'a>) =
        let mutable res = Nil
        for e in s do 
            match res with
                | Nil -> res <- Single e
                | _ -> res <- Concat(res, Single e)
        res

    let rec ofList (l : list<'a>) =
        match l with
            | [] -> Nil
            | h :: t -> Concat(Single h, ofList t)
        
    let rec ofArray (a : 'a[]) =
        if a.Length < 1 then
            Nil
        else
            let mutable res = Single a.[a.Length - 1]
            for i in 2 .. a.Length do
                let i = a.Length - i
                res <- Concat(Single a.[i], res)
            res
        
    let toSeq (c : ConcList<'a>) = c :> seq<_>
    let toList (c : ConcList<'a>) = c |> Seq.toList
    let toArray (c : ConcList<'a>) = c |> Seq.toArray


    let isEmpty (c : ConcList<'a>) =
        match c with
            | Nil -> true
            | _ -> false

    let append (l : ConcList<'a>) (r : ConcList<'a>) =
        match l, r with
            | Nil, r -> r
            | l, Nil -> l
            | l, r -> Concat(l, r)

    let concat (s : #seq<ConcList<'a>>) =
        s |> Seq.fold append Nil
        
    let rec rev (s : ConcList<'a>) =
        match s with
            | Nil -> s
            | Single _ -> s
            | Concat(l,r) -> Concat(rev r, rev l)

    let rec map (f : 'a -> 'b) (l : ConcList<'a>) =
        match l with
            | Nil -> empty
            | Single v -> Single (f v)
            | Concat(l,r) -> Concat(map f l, map f r)

    let rec choose (f : 'a -> Option<'b>) (l : ConcList<'a>) =
        match l with
            | Nil -> 
                empty

            | Single v -> 
                match f v with
                    | Some v -> Single v
                    | None -> empty

            | Concat(l,r) ->
                let l = choose f l
                let r = choose f r
                append l r

    let rec filter (f : 'a -> bool) (l : ConcList<'a>) =
        match l with
            | Nil -> 
                empty

            | Single v -> 
                match f v with
                    | true -> l
                    | _ -> empty

            | Concat(l,r) ->
                let l = filter f l
                let r = filter f r
                append l r
        
    let rec collect (mapping : 'a -> ConcList<'b>) (l : ConcList<'a>) =
        match l with
            | Nil -> empty
            | Single v -> mapping v
            | Concat(l,r) ->
                let l = collect mapping l
                let r = collect mapping r
                append l r
