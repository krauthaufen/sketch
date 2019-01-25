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

module GasacProcs =
    let mutateGenome (g : array<int>) (needed : int) (minLength : int) (maxLength : int) (randIdx : int -> int[]) (mutationProb : float)=
        let ol = g.Length
        let mutable g = g |> Array.copy

        let newLength = 
            if Utils.rand.UniformDouble() < 0.5 then
                Seq.initInfinite ( fun _ -> minLength + Utils.rand.UniformInt(maxLength + 1 - minLength))
                    |> Seq.filter (fun d -> d%needed = 0)
                    |> Seq.head
            else
                ol                

        let g = 
            Array.Resize(&g, newLength)  
            if newLength > ol then
                Seq.initInfinite (fun _ -> 
                    let g = g |> Array.copy
                    let diff = newLength - ol
                    let newIdx = randIdx diff
                    for j in 0..diff-1 do
                        g.[ol+j] <- newIdx.[j]
                    g
                ) |> Seq.filter (fun arr -> not (Utils.hasDuplicates arr))                                      
                  |> Seq.head
            else
                g              

        Seq.initInfinite ( fun _ -> 
            let g = g |> Array.copy
            let ni = randIdx ol
            for j in 0 .. ol-1 do
                if Utils.rand.UniformDouble() < mutationProb then
                    g.[j] <- ni.[j]
            g 
        ) |> Seq.filter ( fun arr -> not (Utils.hasDuplicates arr)) 
          |> Seq.head
          
    let calcScore (models : 'b[]) (test : 't[]) (modelDistance : 't -> 'b -> float) (t : float) =
        if models.Length = 0 then
            System.Double.NegativeInfinity
        else
            let ils =
                models |> Array.collect (fun m -> 
                    test |> Array.filter (fun p -> modelDistance p m < t)
                ) |> Array.distinct
                
            if ils.Length = 0 then
                System.Double.NegativeInfinity
            else   
                let ss = 
                    if models.Length = 1 then
                        let model = models.[0]
                        ils |> Seq.sumBy ( fun il -> 
                            let d = modelDistance il model
                            d / t
                        )
                    else
                        ils |> Seq.sumBy ( fun il -> 
                            let distances = models |> Array.map ( fun m -> modelDistance il m ) |> Array.sort
                            let a = distances.[0]
                            let b = distances.[1]

                            (b - a) / t
                        )
                ss / float ils.Length

type Gasac =
    
    static member solve<'a, 'b, 't when 't : equality>
            (
            p : float, 
            w : float, 
            minModelCount : int,
            maxModelCount : int,
            needed : int, 
            construct :  'a[] -> list<'b>, 
            countInliers : 'b -> 't[] -> int,  
            getInliers : 'b -> 't[] -> int[], 
            getRandomTrainIndices : int -> int[],
            getDistance : 'b -> 't -> float,
            threshold : float,
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
                

        let birthSpecimen (genome : array<int>) = 
            if Utils.hasDuplicates genome then
                None
            else
                let models =
                    genome 
                        |> Array.map ( Array.get train ) 
                        |> construct
                        
                let score =
                    models |> Seq.sumBy (fun p -> countInliers p test |> float)
                    //GasacProcs.calcScore (models |> List.toArray) test (flip getDistance) threshold 

                Some {
                    genome = genome
                    fitness = score
                    solution = models
                }
            
        let newSpecimen() = 
            let len = (minModelCount + (rand.UniformInt(maxModelCount - minModelCount))) * needed
            let gen = getRandomTrainIndices len
            if Utils.hasDuplicates gen then   
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
            let ng = GasacProcs.mutateGenome s.genome needed (needed * minModelCount) (needed * maxModelCount) getRandomTrainIndices mutationProb
            birthSpecimen ng
                 
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
            population 
                |> Seq.sortByDescending (fun s -> s.fitness)
                |> Seq.take (max 1 (population.Count / 5))
                |> Seq.map (fun s -> 
                    let models = construct (s.genome |> Array.map (Array.get train))
                    s,models, GasacProcs.calcScore (models |> List.toArray) test (flip getDistance) threshold, s.fitness
                    )
                |> Seq.sortByDescending (fun (_,_,b,_) -> b)
                |> Seq.head
                |> fun (s,_,_,_) -> s

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
            |> List.map ( fun b -> getInliers b test |> Array.toList )
            |> List.concat
            |> List.toArray

        let bestSol =
            best.genome
            |> Array.map ( Array.get train ) 
            |> construct
        
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

    let solve p w mi ma needed construct countInliers getInliers getRandom dist thresh train test =
        Gasac.solve(p,w,mi,ma,needed,construct,countInliers,getInliers,getRandom,dist,thresh,train,test)



module Test =
    
    let score threshold =
        let rand = RandomSystem()
        let v2r() = rand.UniformV2d(Box2d(V2d.NN,V2d.II))

        let rayToPlane (ray : Ray2d) =
            let p = V2d(ray.Origin.Y, -ray.Origin.X) 
            let n = V2d(ray.Direction.Y, -ray.Direction.X)
            Plane2d(n,p)

        let good = 
            [|
                Ray2d(V2d(0.0,0.0), V2d.OI)
            |] 

        let ps =
            good |> Array.map ( fun ray -> 
                Array.init 25 ( fun _ -> 
                    let t = rand.UniformDouble() * 2.0 - 1.0
                    let p = ray.GetPointOnRay(t)
                    let off = rand.UniformDouble() * 0.25
                    let norm = V2d(ray.Direction.Y, -ray.Direction.X)
                    p + norm * off
                )                
            ) |> Array.concat
            
        let modelDistance (p : V2d) (ray : Ray2d) =
            let plane = rayToPlane ray
            let c = plane.GetClosestPointOn p
            (p-c).Length

        let ils (ps : V2d[]) (rays : Ray2d[]) =
            rays 
            |> Array.map ( fun ray -> 
                ps |> Array.map ( fun p -> modelDistance p ray ) 
            )
            |> fun p -> Log.line "%A" (p |> Array.concat); p
            |> Array.sumBy ( fun rs -> rs |> Array.sumBy (fun f -> if f < threshold then 1.0 else 0.0) )

            
        let bad1 =
            [|
                Ray2d(V2d(0.1,0.0), V2d.OI)
                Ray2d(V2d(0.0,0.0), V2d.OI)
            |] 

        let bad2 =
            [|
                Ray2d(V2d(2.0,0.0), V2d.OI)
                Ray2d(V2d(2.1,0.0), V2d.OI)
            |] 
            
        let bad3 =
            [|
                Ray2d(V2d(0.25,0.0), V2d.OI)
            |] 

        let gil = ils ps good 
        //let b1il = ils ps bad1 
        //let b2il = ils ps bad2
        let b3il = ils ps bad3 

        let gscore  = GasacProcs.calcScore good ps modelDistance threshold
        //let b1score = GasacProcs.calcScore bad1 ps modelDistance threshold
        //let b2score = GasacProcs.calcScore bad2 ps modelDistance threshold
        let b3score = GasacProcs.calcScore bad3 ps modelDistance threshold
        
        printfn "good: %f %A" gil  gscore
        //printfn "bad1: %d %A" b1il b1score
        //printfn "bad2: %d %A" b2il b2score
        printfn "bad3: %f %A" b3il b3score

    let mut() =
        let xs = [|0;1;2;3|]

        let cts = Array.zeroCreate 10

        let getRandom n =
            let ps = Array.init 100 (fun _ -> 1.0)
            Utils.getRandomIndices n ps |> Seq.toArray

        let g = GasacProcs.mutateGenome xs 2 4 8 getRandom 0.5

        printfn "%A" g

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
        let mutable pts = Array.zeroCreate 0
        let mutable superpts = Array.zeroCreate 0
        let mutable probs = Array.zeroCreate 0
        let mutable origplanes = Array.zeroCreate 0

        let addRayWithNoise xoffset =
            let v2r() = rand.UniformV2d(Box2d(V2d.NN,V2d.II)) + (V2d(xoffset,0.0))
            let ray = 
                let p = v2r()
                let d = V2d.OI //rand.UniformV2dDirection()
                Ray2d(p,d)
            let uorigplane = 
                let p = ray.Origin
                let n = V2d(ray.Direction.Y, -ray.Direction.X)
                Plane2d(n,p)
            let usuperpts = 
                let t = rand.UniformDouble() + 0.25
                let p1 = ray.GetPointOnRay(t)
                let p2 = ray.GetPointOnRay(-t)
                [|p1; p2|]
            let upts = 
                let r = 
                    Array.init 500 ( fun _ -> v2r())
                let l = 
                    Array.init 500 (fun _ -> 
                        let t = rand.UniformDouble() * 2.0 - 1.0
                        let p = ray.GetPointOnRay(t)
                        let off = rand.UniformDouble() * 0.25
                        let norm = V2d(ray.Direction.Y, -ray.Direction.X)
                        p + norm * off
                    )
                Array.concat [usuperpts;l;r]
            let uprobs =
                Array.init upts.Length (fun i -> 
                    if i < usuperpts.Length then
                        10.0
                    else
                        1.0                
                )
            pts <- Array.append pts upts
            superpts <- Array.append superpts usuperpts 
            probs <- Array.append probs uprobs 
            origplanes <- Array.append origplanes [|uorigplane|]

        let numModels = 2
        
        for i in 0..numModels-1 do
            addRayWithNoise (3.0 * float i)

        let needed = 2

        let construct(pts : V2d[]) =
            let ct = pts.Length / needed
            List.init ct ( fun i -> 
                let p0 = pts.[2*i] 
                let p1 = pts.[2*i+1]
                let dir = (p1 - p0).Normalized
                let n = V2d(dir.Y, -dir.X) 
                (p0,p1,Plane2d(n,p0))
            )

        let getModelDistance (plane : Plane2d) (p : V2d) =
            let c = plane.GetClosestPointOn p
            (p-c).Length
        
        let getDistance ((p0,p1,ps) : V2d * V2d * Plane2d) (p : V2d) =
            getModelDistance ps p

        let threshold = 0.25
        let inliers (plane : Plane2d) (pts : array<V2d>) =
            pts |> Array.mapi (fun i p -> 
                    let d = getModelDistance plane p
                    i,d
                )
                |> Array.choose(fun (i,d) ->
                    if d < threshold then Some i
                    else None
                )

        let countInliers ((p0,p1,ps) : V2d * V2d * Plane2d) (pts : array<V2d>) = 
            let il = inliers ps pts |> Seq.length
            match superpts |> Seq.contains p0, superpts |> Seq.contains p1 with
            | true, true -> 20 + il
            | false, true | true, false -> 10 + il
            | _ -> il

        let getInliers ((p0,p1,ps) : V2d * V2d * Plane2d) pts = inliers ps pts

        let getRandom (n : int) = Utils.getRandomIndices n probs |> Seq.toArray

        match Gasac.solve 0.999 0.05 numModels numModels needed construct countInliers getInliers getRandom getDistance threshold pts pts with
        | None -> ()
        | Some result ->
            let pls = result.value |> List.map ( fun (_,_,p) -> p )

            Log.line "GASAC: %d iterations, time:%A" result.iterations result.time
            Log.line "input:"
            for p in origplanes do
                Log.line "%A (%A)" p.Normal p.Distance
            Log.line "resul:"
            for p in pls do
                Log.line "%A (%A)" p.Normal p.Distance
            Log.line "inliers: %A" result.inliers.Length
            Log.line "finished"

Test.score 0.2;;
//Test.test();;