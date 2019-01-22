#load "load.fsx"

open System
open System.Collections.Generic
open Aardvark.Base

[<CustomEquality; NoComparison>]
type Specimen<'a> =
    {
        genome : array<int>
        fitness : float
        solution : list<'a>
    } with 
        override x.Equals o =
            match o with
            | :? Specimen<'a> as o -> x.genome = o.genome
            | _ -> false
        override x.GetHashCode() =
            x.genome.GetHashCode()

type RansacResult<'d, 't, 's> = 
    { 
        data            : 'd[]
        test            : 't[]
        value           : 's
        modelIndices    : int[]
        inliers         : int[]
        iterations      : int
        time            : MicroTime 
    }
    member x.model = x.modelIndices |> Array.map (Array.get x.data)
let inline sq a = a * a


module Utils = 
    let rand = RandomSystem()
    let getRandomIndices (n : int) (weights : array<float>) =
        
        let probs = weights.Copy()
        let res = Array.zeroCreate n

        for j in 0..n-1 do
            let pick = rand.UniformDouble() * (probs |> Array.sum)

            let mutable cdf = 0.0
            let mutable found = false
            let mutable i = 0
            while not found do
                cdf <- cdf + probs.[i]
                if pick <= cdf then
                    found <- true
                else            
                    i <- i+1        
            
            res.[j] <- i
            probs.[i] <- 0.0

        res |> Array.toSeq

    let getRandomIndex = getRandomIndices 1 >> Seq.head

type Gasac =
    
    static member solve<'a, 'b, 't>
            (
            p : float, 
            w : float, 
            needed : int, 
            construct :  'a[] -> list<'b>, 
            countInliers : 'b -> 't[] -> int,  
            getInliers : 'b -> 't[] -> int[], 
            getRandomTrainIndices : int -> int[],
            train : 'a[], 
            test : 't[] 
            ) : Option<_> =
        
        let sw = System.Diagnostics.Stopwatch.StartNew()
        Log.startTimed "Gasac"

        let populationLimit = 200
        
        let rand = RandomSystem()
        let population = List<Specimen<_>>(populationLimit+1)
        let cmp = Func<_,_,_>(fun l r -> compare l.fitness r.fitness)
        
        let mutationProb = 1.0/(2.0 * float needed)

        let enqueue a =
            if not (population.Contains a) then
                population.HeapEnqueue(cmp,a)
                if population.Count > populationLimit then
                    population.HeapDequeue(cmp) |> ignore
                
        let hasDuplicates a =
            let rec hasDuplicates (i : int) (j : int) (a : array<_>) =  
                if i >= a.Length then
                    false
                elif j >= a.Length then
                    hasDuplicates (i + 1) (i + 2) a
                else
                    if a.[i] = a.[j] then true
                    else hasDuplicates i (j + 1) a
            hasDuplicates 0 1 a

        let birthSpecimen (genome : array<int>) = 
            if hasDuplicates genome then
                None
            else
                let res =
                    genome 
                        |> Array.map ( Array.get train ) 
                        |> construct
                        |> List.map ( fun b -> b,countInliers b test |> float )

                match res with
                | [] -> None
                | res -> 
                    let fit = res |> List.map snd |> List.min
                    let sol = res |> List.map fst

                    Some {
                        genome = genome
                        fitness = fit
                        solution = sol
                    }
            
        let newSpecimen() = 
            let gen = getRandomTrainIndices 4
            if hasDuplicates gen then   
                None
            else
                birthSpecimen gen

        let combine (l : Specimen<_>) (r : Specimen<_>) =
            let n = min l.genome.Length r.genome.Length
            let a = Array.zeroCreate n
            let b = Array.zeroCreate n
            for i in 0..n-1 do 
                let lg = l.genome.[i]
                let rg = r.genome.[i]
                if rand.UniformInt() &&& 1 = 0 then
                    a.[i] <- lg
                    b.[i] <- rg
                else
                    a.[i] <- rg
                    b.[i] <- lg

            let child1 = birthSpecimen a
            let child2 = birthSpecimen b
            [child1; child2] |> List.choose id
            
        let mutate (s : Specimen<_>) =
            let mutable mutated = s.genome.Copy()
            while not (hasDuplicates mutated) do
                mutated <- s.genome.Copy()
                for i in 0..mutated.Length-1 do
                    if rand.UniformDouble() < mutationProb then
                        mutated.[i] <- (getRandomTrainIndices 1 |> Seq.head)

            birthSpecimen mutated
                 
        let chooseRandomSpecimen() =
            population.[rand.UniformInt(population.Count)]

        let generation () = 
            for _ in 0..populationLimit-1 do
                let p1 = chooseRandomSpecimen()
                let p2 = chooseRandomSpecimen()
        
                combine p1 p2 
                |> List.choose mutate
                |> List.iter enqueue
                
        let best() =
            population |> Seq.maxBy (fun s -> s.fitness)

        Seq.initInfinite ( fun _ -> newSpecimen() ) 
            |> Seq.choose id 
            |> Seq.distinct 
            |> Seq.take populationLimit 
            |> Seq.iter enqueue

        let mutable i = 0
        while i < 20 do  
            Log.line "Generation %d: best=%f" (int i) (best().fitness)
            generation()
            i <- i + 1

        let best = best()
        
        let bestInl = 
            best.genome
            |> Array.map ( Array.get train ) 
            |> construct
            |> List.map ( fun b -> getInliers b test )
            |> List.maxBy Seq.length

        let bestSol =
            best.genome
            |> Array.map ( Array.get train ) 
            |> construct
            |> List.maxBy ( fun b -> countInliers b test |> float)
        
        Log.stop()
        sw.Stop()
        let res = {
            data = train
            test = test
            value = bestSol
            modelIndices = best.genome
            inliers = bestInl
            iterations = int i
            time = sw.MicroTime
        }

        Some res

module Gasac =

    let solve p w needed construct countInliers getInliers getRandom train test =
        Gasac.solve(p,w,needed,construct,countInliers,getInliers,getRandom,train,test)


module Test =
    
    let rnd() =
        let ps = Array.append [|100.0|] (Array.replicate 100 1.0)

        let cts = Array.zeroCreate ps.Length
        let ct = 1000000
        for i in 0..ct-1 do
            let els = Utils.getRandomIndices 5 ps
            for el in els do
                cts.[el] <- cts.[el] + 1

        let cts = cts |> Array.map (fun e -> float e / float ct)
        printfn "probs: "
        for i in 0..cts.Length-1 do
            printfn " %d: %f" i cts.[i]


    let test() =
        let rand = RandomSystem()
        let v2r() = rand.UniformV2d(Box2d(V2d.NN,V2d.II))
        let ray = 
            let p = v2r()
            let d = rand.UniformV2dDirection()
            Ray2d(p,d)

        let origplane = 
            let p = ray.Origin
            let n = V2d(ray.Direction.Y, -ray.Direction.X)
            Plane2d(n,p)
            


        let superpts = 
            let t = rand.UniformDouble() + 0.25
            let p1 = ray.GetPointOnRay(t)
            let p2 = ray.GetPointOnRay(-t)
            [|p1; p2|]
            
        let pts = 
            let r = 
                Array.init 5000 ( fun _ -> v2r())
            let l = 
                Array.init 1000 (fun _ -> 
                    let t = rand.UniformDouble() * 2.0 - 1.0
                    let p = ray.GetPointOnRay(t)
                    let off = rand.UniformDouble() * 0.25
                    let norm = V2d(ray.Direction.Y, -ray.Direction.X)
                    p + norm * off
                )
            Array.concat [superpts;l;r]

        let probs =
            Array.init pts.Length (fun i -> 
                if i < superpts.Length then
                    10.0
                else
                    1.0                
            )

        let construct(pts : V2d[]) =
            let ct = pts.Length / 2
            List.init ct ( fun i -> 
                let p0 = pts.[2*i] 
                let p1 = pts.[2*i+1]
                let dir = (p1 - p0).Normalized
                let n = V2d(dir.Y, -dir.X) 
                (p0,p1,Plane2d(n,p0))
            )

        let inliers (ps : Plane2d) (pts : array<V2d>) =
            let plane = ps
            pts |> Array.mapi (fun i p -> 
                    let c = plane.GetClosestPointOn p
                    let d = (p-c).Length
                    i,d
                )
                |> Array.choose(fun (i,d) ->
                    if d < 0.01 then Some i
                    else None
                )

        let countInliers ((p0,p1,ps) : V2d * V2d * Plane2d) (pts : array<V2d>) = 
            let il = inliers ps pts |> Seq.length
            match superpts |> Seq.contains p0, superpts |> Seq.contains p1 with
            | true, true -> 20 + il
            | false, true | true, false -> 10 + il
            | _ -> il

        let getInliers ((p0,p1,ps) : V2d * V2d * Plane2d) pts = inliers ps pts

        let getRandom (n : int) =
            let ct = Utils.rand.UniformInt(5)+1
            let n = ct * n
            Utils.getRandomIndices n probs |> Seq.toArray

        match Gasac.solve 0.999 0.05 2 construct countInliers getInliers getRandom pts pts with
        | None -> ()
        | Some result ->
            let (_,_,best) = result.value

            Log.line "GASAC: %d iterations, time:%A" result.iterations result.time
            Log.line "input: %A" origplane
            Log.line "resul: %A" best
            Log.line "inliers: %A" result.inliers.Length
            Log.line "finished"


Test.test();;