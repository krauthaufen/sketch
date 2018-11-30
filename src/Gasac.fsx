#load "load.fsx"

open System
open System.Collections.Generic
open Aardvark.Base

type Specimen =
    {
        genome : array<int>
        fitness : float
    } 

//module Normal =
//    let private table =
//        [|
//            0.50000; 0.50399; 0.50798; 0.51197; 0.51595; 0.51994; 0.52392; 0.52790; 0.53188; 0.53586
//            0.53983; 0.54380; 0.54776; 0.55172; 0.55567; 0.55966; 0.56360; 0.56749; 0.57142; 0.57535
//            0.57926; 0.58317; 0.58706; 0.59095; 0.59483; 0.59871; 0.60257; 0.60642; 0.61026; 0.61409
//            0.61791; 0.62172; 0.62552; 0.62930; 0.63307; 0.63683; 0.64058; 0.64431; 0.64803; 0.65173
//            0.65542; 0.65910; 0.66276; 0.66640; 0.67003; 0.67364; 0.67724; 0.68082; 0.68439; 0.68793
//            0.69146; 0.69497; 0.69847; 0.70194; 0.70540; 0.70884; 0.71226; 0.71566; 0.71904; 0.72240
//            0.72575; 0.72907; 0.73237; 0.73565; 0.73891; 0.74215; 0.74537; 0.74857; 0.75175; 0.75490
//            0.75804; 0.76115; 0.76424; 0.76730; 0.77035; 0.77337; 0.77637; 0.77935; 0.78230; 0.78524
//            0.78814; 0.79103; 0.79389; 0.79673; 0.79955; 0.80234; 0.80511; 0.80785; 0.81057; 0.81327
//            0.81594; 0.81859; 0.82121; 0.82381; 0.82639; 0.82894; 0.83147; 0.83398; 0.83646; 0.83891
//            0.84134; 0.84375; 0.84614; 0.84849; 0.85083; 0.85314; 0.85543; 0.85769; 0.85993; 0.86214
//            0.86433; 0.86650; 0.86864; 0.87076; 0.87286; 0.87493; 0.87698; 0.87900; 0.88100; 0.88298
//            0.88493; 0.88686; 0.88877; 0.89065; 0.89251; 0.89435; 0.89617; 0.89796; 0.89973; 0.90147
//            0.90320; 0.90490; 0.90658; 0.90824; 0.90988; 0.91149; 0.91308; 0.91466; 0.91621; 0.91774
//            0.91924; 0.92073; 0.92220; 0.92364; 0.92507; 0.92647; 0.92785; 0.92922; 0.93056; 0.93189
//            0.93319; 0.93448; 0.93574; 0.93699; 0.93822; 0.93943; 0.94062; 0.94179; 0.94295; 0.94408
//            0.94520; 0.94630; 0.94738; 0.94845; 0.94950; 0.95053; 0.95154; 0.95254; 0.95352; 0.95449
//            0.95543; 0.95637; 0.95728; 0.95818; 0.95907; 0.95994; 0.96080; 0.96164; 0.96246; 0.96327
//            0.96407; 0.96485; 0.96562; 0.96638; 0.96712; 0.96784; 0.96856; 0.96926; 0.96995; 0.97062
//            0.97128; 0.97193; 0.97257; 0.97320; 0.97381; 0.97441; 0.97500; 0.97558; 0.97615; 0.97670
//            0.97725; 0.97778; 0.97831; 0.97882; 0.97932; 0.97982; 0.98030; 0.98077; 0.98124; 0.98169
//            0.98214; 0.98257; 0.98300; 0.98341; 0.98382; 0.98422; 0.98461; 0.98500; 0.98537; 0.98574
//            0.98610; 0.98645; 0.98679; 0.98713; 0.98745; 0.98778; 0.98809; 0.98840; 0.98870; 0.98899
//            0.98928; 0.98956; 0.98983; 0.99010; 0.99036; 0.99061; 0.99086; 0.99111; 0.99134; 0.99158
//            0.99180; 0.99202; 0.99224; 0.99245; 0.99266; 0.99286; 0.99305; 0.99324; 0.99343; 0.99361
//            0.99379; 0.99396; 0.99413; 0.99430; 0.99446; 0.99461; 0.99477; 0.99492; 0.99506; 0.99520
//            0.99534; 0.99547; 0.99560; 0.99573; 0.99585; 0.99598; 0.99609; 0.99621; 0.99632; 0.99643
//            0.99653; 0.99664; 0.99674; 0.99683; 0.99693; 0.99702; 0.99711; 0.99720; 0.99728; 0.99736
//            0.99744; 0.99752; 0.99760; 0.99767; 0.99774; 0.99781; 0.99788; 0.99795; 0.99801; 0.99807
//            0.99813; 0.99819; 0.99825; 0.99831; 0.99836; 0.99841; 0.99846; 0.99851; 0.99856; 0.99861
//            0.99865; 0.99869; 0.99874; 0.99878; 0.99882; 0.99886; 0.99889; 0.99893; 0.99896; 0.99900
//            0.99903; 0.99906; 0.99910; 0.99913; 0.99916; 0.99918; 0.99921; 0.99924; 0.99926; 0.99929
//            0.99931; 0.99934; 0.99936; 0.99938; 0.99940; 0.99942; 0.99944; 0.99946; 0.99948; 0.99950
//            0.99952; 0.99953; 0.99955; 0.99957; 0.99958; 0.99960; 0.99961; 0.99962; 0.99964; 0.99965
//            0.99966; 0.99968; 0.99969; 0.99970; 0.99971; 0.99972; 0.99973; 0.99974; 0.99975; 0.99976
//            0.99977; 0.99978; 0.99978; 0.99979; 0.99980; 0.99981; 0.99981; 0.99982; 0.99983; 0.99983
//            0.99984; 0.99985; 0.99985; 0.99986; 0.99986; 0.99987; 0.99987; 0.99988; 0.99988; 0.99989
//            0.99989; 0.99990; 0.99990; 0.99990; 0.99991; 0.99991; 0.99992; 0.99992; 0.99992; 0.99992
//            0.99993; 0.99993; 0.99993; 0.99994; 0.99994; 0.99994; 0.99994; 0.99995; 0.99995; 0.99995
//            0.99995; 0.99995; 0.99996; 0.99996; 0.99996; 0.99996; 0.99996; 0.99996; 0.99997; 0.99997
//            0.99997; 0.99997; 0.99997; 0.99997; 0.99997; 0.99997; 0.99998; 0.99998; 0.99998; 0.99998
//            1.0
//        |]

//    let rec cdf (v : float) =
//        if v = 0.0 then 
//            0.5
//        elif v > 0.0 then
//            if v < 4.1 then
//                let i = (v / 4.1) * (float table.Length - 1.0)
//                let i0 = int i
//                let i1 = i0 + 1
//                let f = i - float i0
//                printfn "%.3f" f
//                let v0 = table.[i0]
//                let v1 = table.[i1]

//                v0 + (v1 - v0) * f
//            else 
//                1.0
//        else
//            1.0 - cdf -v

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
type Gasac =
    
    static member solve<'a, 'b, 't>
            (
            p : float, 
            w : float, 
            needed : int, 
            construct :  'a[] -> list<'b>, 
            countInliers : 'b -> 't[] -> int,  
            getInliers : 'b -> 't[] -> int[], 
            train : 'a[], 
            test : 't[] 
            ) : Option<_> =
        
        let sw = System.Diagnostics.Stopwatch.StartNew()
        Log.startTimed "Gasac"

        let populationLimit = 200
        
        let n = train.Length
        let rand = RandomSystem()
        let population = List<Specimen>(populationLimit+1)
        let cmp = Func<_,_,_>(fun l r -> compare l.fitness r.fitness)
        
        let mutationProb = 1.0/(2.0 * float needed)

        let expectedFitness = w * float train.Length
        
        //    // x ~ N(avg, sqrt var)
        //    // (x - avg) / sqrt var ~ N(0,1)
        //    let x = (f - avg) / sqrt var
        //    1.0 - Normal.cdf x
                
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

                
        let getFitness (genome : array<int>) = 
            if hasDuplicates genome then
                0.0
            else
                genome 
                    |> Array.map ( Array.get train ) 
                    |> construct
                    |> List.map ( fun b -> countInliers b test |> float )
                    |> List.max
                        
        let newSpecimen() = 
            let gen = Array.init needed (fun _ -> rand.UniformInt(n))
            if hasDuplicates gen then   
                None
            else
                {
                    genome = gen
                    fitness = getFitness gen 
                } |> Some

        Seq.initInfinite ( fun _ -> newSpecimen() ) 
            |> Seq.choose id 
            |> Seq.distinct 
            |> Seq.take populationLimit 
            |> Seq.iter enqueue

        let combine (l : Specimen) (r : Specimen) =
            let a = Array.zeroCreate needed
            let b = Array.zeroCreate needed
            for i in 0..needed-1 do 
                let lg = l.genome.[i]
                let rg = r.genome.[i]
                if rand.UniformInt() &&& 1 = 0 then
                    a.[i] <- lg
                    b.[i] <- rg
                else
                    a.[i] <- rg
                    b.[i] <- lg

            let child1 = { genome = a; fitness = getFitness a}
            let child2 = { genome = b; fitness = getFitness b}
            match child1.fitness > 0.0, child2.fitness > 0.0 with
                | true, true    -> [child1; child2]
                | true, _       -> [child1]
                | _, true       -> [child2]
                | _             -> []


        let mutate (s : Specimen) =
            let mutated = s.genome
            for i in 0..needed-1 do
                if rand.UniformDouble() < mutationProb then
                    mutated.[i] <- rand.UniformInt(n)

            let fitness = getFitness mutated
            if fitness > 0.0 then
                Some {
                    genome = mutated
                    fitness = fitness
                }
            else
                None
                 
        let chooseRandomSpecimen() =
            population.[rand.UniformInt(population.Count)]

        let generation () = 
            for _ in 0..populationLimit/2-1 do
                let p1 = chooseRandomSpecimen()
                let p2 = chooseRandomSpecimen()
        
                combine p1 p2 
                |> List.choose mutate
                |> List.iter enqueue


        let best() =
            population |> Seq.maxBy (fun s -> s.fitness)

        let realfitness() =
            let fitnesses =
                population 
                |> Seq.sortByDescending ( fun s -> s.fitness )
                |> Seq.take (populationLimit/8)
                |> Seq.map ( fun s -> 
                    s.genome
                        |> Array.map ( Array.get train ) 
                        |> construct 
                        |> Seq.map(fun b -> countInliers b test |> float)
                        |> Seq.max
                )

            let inline sq a = a * a
            let sum = fitnesses |> Seq.sum
            let avg = sum / float (fitnesses |> Seq.length)
            let var = 
                let t = fitnesses |> Seq.sumBy (fun s -> sq (s - avg))
                t / float ((fitnesses |> Seq.length) - 1)
            avg,var

        let mutable last = -9999.0
        let superfit() =
            //let (realavg,realvar) = realfitness()
            //let diff = sq(realavg - last)
            
            //let best = best()
            //let diff = sq(last - best.fitness)
            //last <- best.fitness

            //let isFit = diff <= best.fitness * (1.0 - p)
            //let difftest = (realavg * (1.0-p))
            //let vartest = realavg * w
            //Log.line "Fitness: avg=%f var=%f(%f) diff=%f(%f)" realavg realvar vartest diff difftest
            
            //diff <= difftest && realvar <= vartest
            
            //isFit
            false


            
        let mutable i = 0.0
        while i < 100.0 do //not (superfit())do  
            Log.line "Generation %d: best=%f" (int i) (best().fitness)
            generation()
            i <- i + 1.0
            
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

    let solve p w needed construct countInliers getInliers train test =
        Gasac.solve(p,w,needed,construct,countInliers,getInliers,train,test)

module Test =
    
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
            
        let evil = 
            Array.init 500 ( fun _ -> v2r() + V2d(2.0,2.0))

        let pts = 
            let r = 
                Array.init 500 ( fun _ -> v2r())
            let l = 
                Array.init 100 (fun _ -> 
                    let t = rand.UniformDouble() * 2.0 - 1.0
                    let p = ray.GetPointOnRay(t)
                    let off = rand.UniformDouble() * 0.25
                    let norm = V2d(ray.Direction.Y, -ray.Direction.X)
                    p + norm * off
                )
            Array.concat [l;r;superpts;evil]

        let construct(pts : V2d[]) =
            let p0 = pts.[0]
            let p1 = pts.[1]
            let dir = (p1 - p0).Normalized
            let n = V2d(dir.Y, -dir.X)
            [(p0,p1,Plane2d(n,p0))]

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
            | true, true -> 1000 + il
            | false, true | true, false -> 500 + il
            | _ -> il

        let getInliers ((p0,p1,ps) : V2d * V2d * Plane2d) pts = inliers ps pts

        match Gasac.solve 0.999999999 0.05 2 construct countInliers getInliers pts pts with
        | None -> ()
        | Some result ->
            let (_,_,best) = result.value

            Log.line "GASAC: %d iterations, time:%A" result.iterations result.time
            Log.line "input: %A" origplane
            Log.line "resul: %A" best
            Log.line "inliers: %A" result.inliers.Length
            Log.line "finished"


Test.test();;