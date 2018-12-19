#load "load.fsx"

open Aardvark.Base
type Normal8 =
    struct
        val mutable public Data : uint8

        member inline x.MaxDim = int (x.Data >>> 6)
        member inline x.X = (x.Data >>> 3) &&& 0x7uy
        member inline x.Y = (x.Data) &&& 0x7uy


        static member ToV3d(x : Normal8) =
            let d = x.MaxDim
            let mutable v = V3d.Zero
            v.[d] <- 1.0
            v.[(d + 1) % 3] <- 2.0 * (float x.X / 7.0) - 1.0
            v.[(d + 2) % 3] <- 2.0 * (float x.Y / 7.0) - 1.0
            v
            
        member x.V3d = Normal8.ToV3d(x)
            
        new(m : int, x : byte, y : byte) =
            {
                Data = ((uint8 m &&& 3uy) <<< 6) ||| ((uint8 x &&& 0x7uy) <<< 3) ||| (uint8 y &&& 0x7uy)
            }

        new(v : V3d) =
            let d = v.Abs.MajorDim
            let v = v / v.[d]
            let x = 0.5 * (v.[(d + 1) % 3] + 1.0) * 7.0 + 0.5 |> byte
            let y = 0.5 * (v.[(d + 2) % 3] + 1.0) * 7.0 + 0.5 |> byte
            Normal8(d, x, y)
            
    end


type Normal16 =
    struct
        val mutable public Data : uint16

        member inline x.MaxDim = int (x.Data >>> 14)
        member inline x.X = (x.Data >>> 7) &&& 0x7Fus
        member inline x.Y = (x.Data) &&& 0x7Fus


        static member ToV3d(x : Normal16) =
            let d = x.MaxDim
            let mutable v = V3d.Zero
            v.[d] <- 1.0
            v.[(d + 1) % 3] <- 2.0 * (float x.X / 126.0) - 1.0
            v.[(d + 2) % 3] <- 2.0 * (float x.Y / 126.0) - 1.0
            v
        member x.V3d = Normal16.ToV3d(x)
            
        new(m : int, x : byte, y : byte) =
            {
                Data = ((uint16 m &&& 3us) <<< 14) ||| ((uint16 x &&& 0x7Fus) <<< 7) ||| (uint16 y &&& 0x7Fus)
            }

        new(v : V3d) =
            let d = v.Abs.MajorDim
            let v = v / v.[d]
            let x = 0.5 * (v.[(d + 1) % 3] + 1.0) * 126.0 + 0.5 |> byte
            let y = 0.5 * (v.[(d + 2) % 3] + 1.0) * 126.0 + 0.5 |> byte
            Normal16(d, x, y)
            
    end

type Normal32 =
    struct
        val mutable public Data : uint32

        member inline x.MaxDim = int (x.Data >>> 30)
        member inline x.X = (x.Data >>> 15) &&& 0x7FFFu
        member inline x.Y = (x.Data) &&& 0x7FFFu

        
        static member ToV3d(x : Normal32) =
            let d = x.MaxDim
            let mutable v = V3d.Zero
            v.[d] <- 1.0
            v.[(d + 1) % 3] <- 2.0 * (float x.X / 32766.0) - 1.0
            v.[(d + 2) % 3] <- 2.0 * (float x.Y / 32766.0) - 1.0
            v

        member x.V3d = Normal32.ToV3d(x)
            
        new(m : int, x : uint16, y : uint16) =
            {
                Data = ((uint32 m &&& 3u) <<< 30) ||| ((uint32 x &&& 0x7FFFu) <<< 15) ||| (uint32 y &&& 0x7FFFu)
            }

        new(v : V3d) =
            let d = v.Abs.MajorDim
            let v = v / v.[d]
            let x = 0.5 * (v.[(d + 1) % 3] + 1.0) * 32766.0 + 0.5 |> uint16
            let y = 0.5 * (v.[(d + 2) % 3] + 1.0) * 32766.0 + 0.5 |> uint16
            Normal32(d, x, y)
            
    end

let test (name : string) (there : V3d -> 'a) (back : 'a -> V3d) =
    let rand = RandomSystem()

    let mutable sum = 0.0
    let mutable h = 0.0
    let mutable l = System.Double.PositiveInfinity
    let mutable count = 0
    for i in 1 .. 1000000 do
        let v = rand.UniformV3dDirection().Normalized
        let vd = there(v)
        let d = (back vd).Normalized

        let angle = acos (abs (Vec.dot v d) |> clamp 0.0 1.0)
        //if angle > 5.0 * Constant.RadiansPerDegree then
        //    printfn "%A vs %A" v (vd.ToV3d())
        //    printfn "%A %A %A" vd.MaxDim vd.X vd.Y
        //    printfn "%.3f°" (Constant.DegreesPerRadian * angle)
        l <- min angle l
        h <- max angle h
        sum <- sum + angle
        count <- count + 1
    printfn "%s" name
    printfn "  avg: %.3f°" (Constant.DegreesPerRadian * (sum / float count))
    printfn "  min: %.3f°" (Constant.DegreesPerRadian * l)
    printfn "  max: %.3f°" (Constant.DegreesPerRadian * h)


let test8() = test Normal8 Normal8.ToV3d
let test32() = test Normal16 Normal16.ToV3d
let test32() = test Normal32 Normal32.ToV3d
