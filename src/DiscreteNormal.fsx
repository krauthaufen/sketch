#load "load.fsx"

open Aardvark.Base
open MBrace.FsPickler
open MBrace.FsPickler.Combinators
open System.Runtime.InteropServices

#nowarn "9"

[<AutoOpen>]
module private NormalHelpers =

    let inline n(d : int) =
        if d = 2 then 0
        else d + 1

    let inline nn(d : int) =
        if d = 0 then 2
        else d - 1

    module Vec =
        let inline angleBetween (l : ^a) (r : ^a) =
            let l = (^a : (member Normalized : ^a) (l))
            let r = (^a : (member Normalized : ^a) (r))
            let c = (^a : (static member Cross : ^a * ^a -> ^a) (l,r))
            let l = (^a : (member Length : ^b) (c))
            asin (min LanguagePrimitives.GenericOne l)


let test (name : string) (there : V3d -> 'a) (back : 'a -> V3d) =
    let rand = RandomSystem()

    let mutable sum = 0.0
    let mutable h = 0.0
    let mutable l = System.Double.PositiveInfinity
    let mutable count = 0
    for i in 1 .. 1000000 do
        let v = rand.UniformV3dDirection().Normalized
        let vd = there(v)
        let d = back vd

        let angle = Vec.angleBetween v d
        if angle > 5.0 * Constant.RadiansPerDegree then
            printfn "%A vs %A" v d
            printfn "%A" vd
            printfn "%.3f°" (Constant.DegreesPerRadian * angle)
        l <- min angle l
        h <- max angle h
        sum <- sum + angle
        count <- count + 1
    printfn "%s" name
    printfn "  min: %.6e°" (Constant.DegreesPerRadian * l)
    printfn "  max: %.6e°" (Constant.DegreesPerRadian * h)
    printfn "  avg: %.6e°" (Constant.DegreesPerRadian * (sum / float count))



[<ReflectedDefinition>]
module Normal16 =
    let private sgn (v : V2d) = V2d((if v.X >= 0.0 then 1.0 else -1.0), (if v.Y >= 0.0 then 1.0 else -1.0))
    let private clamp (v : V2d) =
        V2d(
            (if v.X < -1.0 then -1.0 elif v.X > 1.0 then 1.0 else v.X),
            (if v.Y < -1.0 then -1.0 elif v.Y > 1.0 then 1.0 else v.Y)
        )

    let decode (v : uint16) : V3d =
        let e = V2d(float (v >>> 8) / 255.0, float (v &&& 0xFFus) / 255.0) * 2.0 - V2d.II
        let v = V3d(e, 1.0 - abs e.X - abs e.Y)
        if v.Z < 0.0 then V3d(V2d(1.0 - abs v.Y, 1.0 - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
        else v |> Vec.normalize

    let encode (v : V3d) : uint16 =
        let p = v.XY * (1.0 / (abs v.X + abs v.Y + abs v.Z))
        let p = 
            if v.Z <= 0.0 then clamp (V2d(1.0 - abs p.Y, 1.0 - abs p.X) * sgn p)
            else clamp p
        
        let x0 = floor ((p.X * 0.5 + 0.5) * 255.0) |> int
        let y0 = floor ((p.Y * 0.5 + 0.5) * 255.0) |> int

        let mutable bestDot = 0.0
        let mutable best = 0us

        for dx in 0 .. 1 do
            for dy in 0 .. 1 do
                let e = uint16 (((x0 + dx) <<< 8) ||| (y0 + dy))
                let vv = decode e
                let d = Vec.dot vv v
                if d > bestDot then
                    bestDot <- d
                    best <- e

        best
        
[<ReflectedDefinition>]
module Normal32 =
    let private sgn (v : V2d) = V2d((if v.X >= 0.0 then 1.0 else -1.0), (if v.Y >= 0.0 then 1.0 else -1.0))
    let private clamp (v : V2d) =
        V2d(
            (if v.X < -1.0 then -1.0 elif v.X > 1.0 then 1.0 else v.X),
            (if v.Y < -1.0 then -1.0 elif v.Y > 1.0 then 1.0 else v.Y)
        )

    let decode (v : uint32) : V3d =
        let e = V2d(float (v >>> 16) / 65535.0, float (v &&& 0xFFFFu) / 65535.0) * 2.0 - V2d.II
        let v = V3d(e, 1.0 - abs e.X - abs e.Y)
        if v.Z < 0.0 then V3d(V2d(1.0 - abs v.Y, 1.0 - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
        else v |> Vec.normalize

    let encode (v : V3d) : uint32 =
        let p = v.XY * (1.0 / (abs v.X + abs v.Y + abs v.Z))
        let p = 
            if v.Z <= 0.0 then clamp (V2d(1.0 - abs p.Y, 1.0 - abs p.X) * sgn p)
            else clamp p
        
        let x0 = floor ((p.X * 0.5 + 0.5) * 65535.0) |> int
        let y0 = floor ((p.Y * 0.5 + 0.5) * 65535.0) |> int

        let mutable bestDot = 0.0
        let mutable best = 0u

        for dx in 0 .. 1 do
            for dy in 0 .. 1 do
                let e = uint32 (((x0 + dx) <<< 16) ||| (y0 + dy))
                let vv = decode e
                let d = Vec.dot vv v
                if d > bestDot then
                    bestDot <- d
                    best <- e

        best

[<ReflectedDefinition>]
module Normal64 =
    let private sgn (v : V2d) = V2d((if v.X >= 0.0 then 1.0 else -1.0), (if v.Y >= 0.0 then 1.0 else -1.0))
    let private clamp (v : V2d) =
        V2d(
            (if v.X < -1.0 then -1.0 elif v.X > 1.0 then 1.0 else v.X),
            (if v.Y < -1.0 then -1.0 elif v.Y > 1.0 then 1.0 else v.Y)
        )
        
    let decode (v : uint64) : V3d =
        let e = V2d(float (v >>> 32) / 4294967295.0, float (v &&& 0xFFFFFFFFUL) / 4294967295.0) * 2.0 - V2d.II
        let v = V3d(e, 1.0 - abs e.X - abs e.Y)
        if v.Z < 0.0 then V3d(V2d(1.0 - abs v.Y, 1.0 - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
        else v |> Vec.normalize
        
    let encode (v : V3d) : uint64 =
        let p = v.XY * (1.0 / (abs v.X + abs v.Y + abs v.Z))
        let p = 
            if v.Z <= 0.0 then clamp (V2d(1.0 - abs p.Y, 1.0 - abs p.X) * sgn p)
            else clamp p
                
        let x0 = floor ((p.X * 0.5 + 0.5) * 4294967295.0) |> int64
        let y0 = floor ((p.Y * 0.5 + 0.5) * 4294967295.0) |> int64
        
        let mutable bestDot = 0.0
        let mutable best = 0UL
        
        for dx in 0L .. 1L do
            for dy in 0L .. 1L do
                let e = uint64 (((x0 + dx) <<< 32) ||| (y0 + dy))
                let vv = decode e
                let d = Vec.dot vv v
                if d > bestDot then
                    bestDot <- d
                    best <- e
        
        best


let run() = 
    test "oct16P" Normal16.encode Normal16.decode
    test "oct32P" Normal32.encode Normal32.decode
    test "oct64P" Normal64.encode Normal64.decode
    // oct16P
    //     min: 5.794158e-004°
    //     max: 6.313213e-001°
    //     avg: 3.134081e-001°
    // oct32P
    //     min: 2.687520e-006°
    //     max: 2.458537e-003°
    //     avg: 1.219139e-003°
    // oct64P
    //     min: 1.265358e-010°
    //     max: 1.120909e-007°
    //     avg: 3.983056e-008°


