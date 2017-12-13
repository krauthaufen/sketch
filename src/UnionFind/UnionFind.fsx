#load @"..\load.fsx"
#load "ConcList.fsx"

open ConcList
open System.Collections.Generic
open Aardvark.Base


type Edge<'n, 'w> =
    {
        left    : 'n
        right   : 'n
        cost    : 'w
    }

module FeatureGraph =

    type private UnionNode<'n, 'c, 'w when 'c : comparison> =
        class
            val mutable public height : int
            val mutable public parent : Option<UnionNode<'n, 'c, 'w>>
            val mutable public colors : Set<'c>
            val mutable public edges  : ConcList<Edge<'n, 'w>>

            new(color) = { height = 1; parent = None; colors = Set.singleton color; edges = ConcList.empty }
        end

    type private UnionFind<'n, 'c, 'w when 'c : comparison>(getColor : 'n -> 'c) =
        let nodes = Dict<'n, UnionNode<'n, 'c, 'w>>()
        let roots = HashSet<UnionNode<'n, 'c, 'w>>()

        let getNode (n : 'n) =
            nodes.GetOrCreate(n, fun n ->
                let n = UnionNode(getColor n)
                roots.Add n |> ignore
                n
            )

        let rec rep (node : UnionNode<'n, 'c, 'w>) =
            match node.parent with
                | Some p -> 
                    let r = rep p
                    if r <> p then
                        node.parent <- Some r
                    r
                | None -> 
                    node

        let rec merge (child : UnionNode<'n, 'c, 'w>) (parent : UnionNode<'n, 'c, 'w>) =
            if child.height > parent.height then
                merge parent child
            else
                roots.Remove child |> ignore
                child.parent <- Some parent
                parent.colors <- Set.union child.colors parent.colors
                parent.edges <- ConcList.append child.edges parent.edges
                parent.height <- max (1 + child.height) parent.height
                parent

        member x.Add(edge : Edge<'n, 'w>) =
            let l = getNode edge.left
            let r = getNode edge.right
            
            let lr = rep l
            let rr = rep r

            if lr <> rr then
                let distinct = Set.intersect lr.colors rr.colors |> Set.isEmpty
                if distinct then    
                    let parent = merge lr rr
                    parent.edges <- ConcList.append parent.edges (ConcList.singleton edge)
                    true
                else
                    false
            else
                false

        member x.Trees =
            roots |> Seq.toList |> List.map (fun r ->
                let edges = ConcList.toList r.edges
                let colors = r.colors
                colors, edges
            )

    let buildTrees (color : 'n -> 'c) (compare : 'w -> 'w -> int) (edges : seq<Edge<'n, 'w>>) =
        let edges = Seq.toArray edges
        edges.QuickSort(System.Func<_,_,_>(fun l r -> compare l.cost r.cost))

        let uf = UnionFind<'n, 'c, 'w>(color)

        for e in edges do
            uf.Add e |> ignore

        uf.Trees

let test() =    
    let simpleEdge (a : V2d) (b : V2d) =
        { left = a; right = b; cost = V2d.Distance(a,b) }

    let colors = 
        HMap.ofList [
            V2d.OO, 0
            V2d.IO, 1
            V2d.II, 2
            V2d.OI, 3
            (V2d(0.0, 0.5)), 0
        ]

    let edges =
        [
            simpleEdge V2d.OO V2d.IO
            simpleEdge V2d.IO V2d.II
            simpleEdge V2d.II V2d.OI
            simpleEdge V2d.OI (V2d(0.0, 0.5))
        ]
    let trees = 
        FeatureGraph.buildTrees (fun v -> HMap.find v colors) compare edges

    printfn "%A" trees