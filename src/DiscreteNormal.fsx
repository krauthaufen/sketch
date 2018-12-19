#load "load.fsx"

open Aardvark.Base


type DiscreteNormal =
    struct
        val mutable public Data : uint16

        member x.MaxDim = int (x.Data >>> 14)
        member x.X = (x.Data >>> 7) &&& 0x7Fus
        member x.Y = (x.Data) &&& 0x7Fus


        member x.ToV3d() =
            let d = x.MaxDim
            let mutable v = V3d.Zero
            v.[d] <- 1.0
            v.[(d + 1) % 3] <- 2.0 * (float x.X / 126.0) - 1.0
            v.[(d + 2) % 3] <- 2.0 * (float x.Y / 126.0) - 1.0
            v




        new(m : int, x : byte, y : byte) =
            {
                Data = ((uint16 m &&& 3us) <<< 14) ||| ((uint16 x &&& 0x7Fus) <<< 7) ||| (uint16 y &&& 0x7Fus)
            }

        new(v : V3d) =
            let d = v.Abs.MajorDim
            let v = v / v.[d]
            let x = 0.5 * (v.[(d + 1) % 3] + 1.0) * 126.0 |> byte
            let y = 0.5 * (v.[(d + 2) % 3] + 1.0) * 126.0 |> byte
            DiscreteNormal(d, x, y)


    end


let test() =
    let rand = RandomSystem()

    let mutable sum = 0.0
    let mutable h = 0.0
    let mutable l = System.Double.PositiveInfinity
    let mutable count = 0
    for i in 1 .. 1000000 do
        let v = rand.UniformV3dDirection().Normalized
        let vd = DiscreteNormal(v)
        let d = vd.ToV3d().Normalized

        let angle = acos (abs (Vec.dot v d) |> clamp 0.0 1.0)
        if angle > 5.0 * Constant.RadiansPerDegree then
            printfn "%A vs %A" v (vd.ToV3d())
            printfn "%A %A %A" vd.MaxDim vd.X vd.Y
            printfn "%.3f°" (Constant.DegreesPerRadian * angle)
        l <- min angle l
        h <- max angle h
        sum <- sum + angle
        count <- count + 1
        
    printfn "avg: %.3f°" (Constant.DegreesPerRadian * (sum / float count))
    printfn "min: %.3f°" (Constant.DegreesPerRadian * l)
    printfn "max: %.3f°" (Constant.DegreesPerRadian * h)
