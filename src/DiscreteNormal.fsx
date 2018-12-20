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

[<CustomPickler; StructuredFormatDisplay("{AsString}"); StructLayout(LayoutKind.Sequential)>]
type normal16 =
    struct
        val mutable public Data : uint16

        member inline x.MaxDim = int (x.Data >>> 14) - 1
        member inline x.X = (x.Data >>> 7) &&& 0x7Fus
        member inline x.Y = x.Data &&& 0x7Fus
        
        static member inline private Encode(v : V3d) =
            if V3d.ApproxEqual(v, V3d.Zero) then
                0us
            else
                let d = v.Abs.MajorDim
                let v = v / v.[d]
                let x = (63.5 * (v.[n d] + 1.0)) + 0.5 |> byte
                let y = (63.5 * (v.[nn d] + 1.0)) + 0.5 |> byte
                (uint16 (d + 1) <<< 14) ||| (uint16 x <<< 7) ||| (uint16 y)
            
        static member inline private Encode(v : V3f) =
            if V3f.ApproxEqual(v, V3f.Zero) then
                0us
            else
                let d = v.Abs.MajorDim
                let v = v / v.[d]
                let x = (63.5f * (v.[n d] + 1.0f)) + 0.5f |> byte
                let y = (63.5f * (v.[nn d] + 1.0f)) + 0.5f |> byte
                (uint16 (d + 1) <<< 14) ||| (uint16 x <<< 7) ||| (uint16 y)

        static member private CreatePickler(r : IPicklerResolver) =
            let read (rs : ReadState) =
                normal16(Pickler.uint16.Read rs "Data")

            let write (ws : WriteState) (n : normal16) =
                Pickler.uint16.Write ws "Data" n.Data

            let clone (cs : CloneState) (n : normal16) =
                n

            let accept (a : VisitState) (n : normal16) =
                Pickler.uint16.Accept a n.Data

            Pickler.FromPrimitives(read, write, clone, accept)

        member private x.AsString = x.ToString()

        static member Zero = normal16(0us)

        static member MaxErrorInDegrees =
            let s = 1.0 / 128.0
            atan (sqrt (2.0*s*s)) * Constant.DegreesPerRadian

        static member ToV3d(x : normal16) =
            let d = x.MaxDim
            if d < 0 then
                V3d.Zero
            else
                let mutable v = V3d.Zero
                v.[d] <- 1.0
                v.[n d] <- float x.X / 63.5 - 1.0
                v.[nn d] <- float x.Y / 63.5 - 1.0
                v.Normalize()
                v

        static member ToV3f(x : normal16) =
            let d = x.MaxDim
            if d < 0 then
                V3f.Zero
            else
                let mutable v = V3f.Zero
                v.[d] <- 1.0f
                v.[n d] <- float32 x.X / 63.5f - 1.0f
                v.[nn d] <- float32 x.Y / 63.5f - 1.0f
                v.Normalize()
                v

        member x.V3f 
            with inline get() = normal16.ToV3f(x)
            and set (v : V3f) = x.Data <- normal16.Encode v

        member x.V3d 
            with inline get() = normal16.ToV3d(x)
            and set (v : V3d) = x.Data <- normal16.Encode v

        static member op_Explicit(x : normal16) = normal16.ToV3f(x)
        static member op_Explicit(x : normal16) = normal16.ToV3d(x)
            
        static member Cross(l : normal16, r : normal16) = normal16(Vec.cross l.V3d r.V3d)
        static member Dot(l : normal16, r : normal16) = Vec.dot l.V3d r.V3d
        static member Dot(l : normal16, r : V3d) = Vec.dot l.V3d r
        static member Dot(l : V3d, r : normal16) = Vec.dot l r.V3d
        static member Dot(l : normal16, r : V3f) = Vec.dot l.V3f r
        static member Dot(l : V3f, r : normal16) = Vec.dot l r.V3f

        static member AngleBetween(l : normal16, r : normal16) = Vec.angleBetween l.V3d r.V3d
        static member AngleBetween(l : normal16, r : V3d) = Vec.angleBetween l.V3d r
        static member AngleBetween(l : V3d, r : normal16) = Vec.angleBetween l r.V3d
        static member AngleBetween(l : normal16, r : V3f) = Vec.angleBetween l.V3f r
        static member AngleBetween(l : V3f, r : normal16) = Vec.angleBetween l r.V3f

        override x.ToString() =
            let v = x.V3d
            sprintf "[%.3f, %.3f, %.3f]" v.X v.Y v.Z

        

        new (data : uint16) = { Data = data }
        new (v : V3d) = { Data = normal16.Encode v }
        new (v : V3f) = { Data = normal16.Encode v }
        new (x : float, y : float, z : float) = normal16(V3d(x,y,z))
        new (x : float32, y : float32, z : float32) = normal16(V3d(x,y,z))
        new (phi : float, theta : float) = normal16(V2d(phi,theta).CartesianFromSpherical())
        new (phi : float32, theta : float32) = normal16(V2d(phi,theta).CartesianFromSpherical())
    end
    
[<CustomPickler; StructuredFormatDisplay("{AsString}"); StructLayout(LayoutKind.Sequential)>]
type normal32 =
    struct
        val mutable public Data : uint32

        member inline x.MaxDim = int (x.Data >>> 30) - 1
        member inline x.X = (x.Data >>> 15) &&& 0x7FFFu
        member inline x.Y = (x.Data) &&& 0x7FFFu
        
        static member inline private Encode(v : V3d) =
            if V3d.ApproxEqual(v, V3d.Zero) then
                0u
            else
                let d = v.Abs.MajorDim
                let v = v / v.[d]
                let x = 16383.5 * (v.[n d] + 1.0) + 0.5
                let y = 16383.5 * (v.[nn d] + 1.0) + 0.5
                (uint32 (1 + d) <<< 30) ||| (uint32 x <<< 15) ||| (uint32 y)
            
        static member inline private Encode(v : V3f) =
            if V3f.ApproxEqual(v, V3f.Zero) then
                0u
            else
                let d = v.Abs.MajorDim
                let v = v / v.[d]
                let x = 16383.5f * (v.[n d] + 1.0f) + 0.5f
                let y = 16383.5f * (v.[nn d] + 1.0f) + 0.5f
                (uint32 (1 + d) <<< 30) ||| (uint32 x <<< 15) ||| (uint32 y)
            
        static member MaxErrorInDegrees =
            let s = 1.0 / 32768.0
            atan (sqrt (2.0*s*s)) * Constant.DegreesPerRadian


        static member private CreatePickler(r : IPicklerResolver) =
            let read (rs : ReadState) =
                normal32(Pickler.uint32.Read rs "Data")

            let write (ws : WriteState) (n : normal32) =
                Pickler.uint32.Write ws "Data" n.Data

            let clone (cs : CloneState) (n : normal32) =
                n

            let accept (a : VisitState) (n : normal32) =
                Pickler.uint32.Accept a n.Data

            Pickler.FromPrimitives(read, write, clone, accept)

        static member ToV3d(x : normal32) =
            let d = x.MaxDim
            if d < 0 then
                V3d.Zero
            else 
                let mutable v = V3d.Zero
                v.[d] <- 1.0
                v.[n d] <- (float x.X / 16383.5) - 1.0
                v.[nn d] <- (float x.Y / 16383.5) - 1.0
                v.Normalize()
                v

        static member ToV3f(x : normal32) =
            let d = x.MaxDim
            if d < 0 then
                V3f.Zero
            else 
                let mutable v = V3f.Zero
                v.[d] <- 1.0f
                v.[n d] <- (float32 x.X / 16383.5f) - 1.0f
                v.[nn d] <- (float32 x.Y / 16383.5f) - 1.0f
                v.Normalize()
                v
                
        static member op_Explicit(x : normal32) = normal32.ToV3f(x)
        static member op_Explicit(x : normal32) = normal32.ToV3d(x)

        member x.V3d 
            with inline get() = normal32.ToV3d(x)
            and set (v : V3d) = x.Data <- normal32.Encode v
        member x.V3f 
            with inline get() = normal32.ToV3f(x)
            and set (v : V3f) = x.Data <- normal32.Encode v
        
        member private x.AsString = x.ToString()
        
        static member Cross(l : normal32, r : normal32) = normal32(Vec.cross l.V3d r.V3d)
        static member Dot(l : normal32, r : normal32) = Vec.dot l.V3d r.V3d
        static member Dot(l : normal32, r : V3d) = Vec.dot l.V3d r
        static member Dot(l : V3d, r : normal32) = Vec.dot l r.V3d
        static member Dot(l : normal32, r : V3f) = Vec.dot l.V3f r
        static member Dot(l : V3f, r : normal32) = Vec.dot l r.V3f
        static member Dot(l : normal32, r : normal16) = Vec.dot l.V3d r.V3d
        static member Dot(l : normal16, r : normal32) = Vec.dot l.V3d r.V3d
        
        static member AngleBetween(l : normal32, r : normal32) = Vec.angleBetween l.V3d r.V3d
        static member AngleBetween(l : normal32, r : V3d) = Vec.angleBetween l.V3d r
        static member AngleBetween(l : V3d, r : normal32) = Vec.angleBetween l r.V3d
        static member AngleBetween(l : normal32, r : V3f) = Vec.angleBetween l.V3f r
        static member AngleBetween(l : V3f, r : normal32) = Vec.angleBetween l r.V3f
        static member AngleBetween(l : normal32, r : normal16) = Vec.angleBetween l.V3d r.V3d
        static member AngleBetween(l : normal16, r : normal32) = Vec.angleBetween l.V3d r.V3d


        override x.ToString() =
            let v = x.V3d
            sprintf "[%.5f, %.5f, %.5f]" v.X v.Y v.Z

        new(data : uint32) = { Data = data }
        
        new(v : V3d) = { Data = normal32.Encode v }
        new(v : V3f) = { Data = normal32.Encode v }
        new (x : float, y : float, z : float) = normal32(V3d(x,y,z))
        new (x : float32, y : float32, z : float32) = normal32(V3d(x,y,z))
        new (phi : float, theta : float) = normal32(V2d(phi,theta).CartesianFromSpherical())
        new (phi : float32, theta : float32) = normal32(V2d(phi,theta).CartesianFromSpherical())
    end
    
[<CustomPickler; StructuredFormatDisplay("{AsString}"); StructLayout(LayoutKind.Sequential)>]
type normal64 =
    struct
        val mutable public Data : uint64

        member inline x.MaxDim = int (x.Data >>> 62) - 1
        member inline x.X = (x.Data >>> 31) &&& 0x7FFFFFFFUL
        member inline x.Y = (x.Data) &&& 0x7FFFFFFFUL
        
        static member inline private Encode(v : V3d) =
            if V3d.ApproxEqual(v, V3d.Zero) then
                0UL
            else
                let d = v.Abs.MajorDim
                let v = v / v.[d]
                let x = 1073741823.5 * (v.[n d] + 1.0) + 0.5
                let y = 1073741823.5 * (v.[nn d] + 1.0) + 0.5
                (uint64 (1 + d) <<< 62) ||| (uint64 x <<< 31) ||| (uint64 y)
            
        static member inline private Encode(v : V3f) =
            if V3f.ApproxEqual(v, V3f.Zero) then
                0UL
            else
                let d = v.Abs.MajorDim
                let v = v / v.[d]
                let x = 1073741823.5 * (float v.[n d] + 1.0) + 0.5
                let y = 1073741823.5 * (float v.[nn d] + 1.0) + 0.5
                (uint64 (1 + d) <<< 62) ||| (uint64 x <<< 31) ||| (uint64 y)
            
        static member MaxErrorInDegrees =
            let s = 1.0 / 1073741823.5
            atan (sqrt (2.0*s*s)) * Constant.DegreesPerRadian


        static member private CreatePickler(r : IPicklerResolver) =
            let read (rs : ReadState) =
                normal64(Pickler.uint64.Read rs "Data")

            let write (ws : WriteState) (n : normal64) =
                Pickler.uint64.Write ws "Data" n.Data

            let clone (cs : CloneState) (n : normal64) =
                n

            let accept (a : VisitState) (n : normal64) =
                Pickler.uint64.Accept a n.Data

            Pickler.FromPrimitives(read, write, clone, accept)

        static member ToV3d(x : normal64) =
            let d = x.MaxDim
            if d < 0 then
                V3d.Zero
            else 
                let mutable v = V3d.Zero
                v.[d] <- 1.0
                v.[n d] <- (float x.X / 1073741823.5) - 1.0
                v.[nn d] <- (float x.Y / 1073741823.5) - 1.0
                v.Normalize()
                v

        static member ToV3f(x : normal64) =
            let d = x.MaxDim
            if d < 0 then
                V3f.Zero
            else 
                let mutable v = V3f.Zero
                v.[d] <- 1.0f
                v.[n d] <- (float x.X / 1073741823.5) - 1.0 |> float32
                v.[nn d] <- (float x.Y / 1073741823.5) - 1.0 |> float32
                v.Normalize()
                v
                
        static member op_Explicit(x : normal64) = normal64.ToV3f(x)
        static member op_Explicit(x : normal64) = normal64.ToV3d(x)

        member x.V3d 
            with inline get() = normal64.ToV3d(x)
            and set (v : V3d) = x.Data <- normal64.Encode v
        member x.V3f 
            with inline get() = normal64.ToV3f(x)
            and set (v : V3f) = x.Data <- normal64.Encode v
        
        member private x.AsString = x.ToString()
        
        static member Cross(l : normal64, r : normal64) = normal64(Vec.cross l.V3d r.V3d)
        static member Dot(l : normal64, r : normal64) = Vec.dot l.V3d r.V3d
        static member Dot(l : normal64, r : normal16) = Vec.dot l.V3d r.V3d
        static member Dot(l : normal16, r : normal64) = Vec.dot l.V3d r.V3d
        static member Dot(l : normal64, r : normal32) = Vec.dot l.V3d r.V3d
        static member Dot(l : normal32, r : normal64) = Vec.dot l.V3d r.V3d
        static member Dot(l : normal64, r : V3d) = Vec.dot l.V3d r
        static member Dot(l : V3d, r : normal64) = Vec.dot l r.V3d
        static member Dot(l : normal64, r : V3f) = Vec.dot l.V3f r
        static member Dot(l : V3f, r : normal64) = Vec.dot l r.V3f

        static member AngleBetween(l : normal64, r : normal64) = Vec.angleBetween l.V3d r.V3d
        static member AngleBetween(l : normal64, r : V3d) = Vec.angleBetween l.V3d r
        static member AngleBetween(l : V3d, r : normal64) = Vec.angleBetween l r.V3d
        static member AngleBetween(l : normal64, r : V3f) = Vec.angleBetween l.V3f r
        static member AngleBetween(l : V3f, r : normal64) = Vec.angleBetween l r.V3f
        static member AngleBetween(l : normal64, r : normal16) = Vec.angleBetween l.V3d r.V3d
        static member AngleBetween(l : normal16, r : normal64) = Vec.angleBetween l.V3d r.V3d
        static member AngleBetween(l : normal64, r : normal32) = Vec.angleBetween l.V3d r.V3d
        static member AngleBetween(l : normal32, r : normal64) = Vec.angleBetween l.V3d r.V3d


        override x.ToString() =
            let v = x.V3d
            sprintf "[%.5f, %.5f, %.5f]" v.X v.Y v.Z

        new(data : uint64) = { Data = data }
        
        new(v : V3d) = { Data = normal64.Encode v }
        new(v : V3f) = { Data = normal64.Encode v }
        new (x : float, y : float, z : float) = normal64(V3d(x,y,z))
        new (x : float32, y : float32, z : float32) = normal64(V3d(x,y,z))
        new (phi : float, theta : float) = normal64(V2d(phi,theta).CartesianFromSpherical())
        new (phi : float32, theta : float32) = normal64(V2d(phi,theta).CartesianFromSpherical())
    end

module Normal =
    let inline toV3f (n : ^a) = (^a : (static member ToV3f : ^a -> V3f) (n))
    let inline toV3d (n : ^a) = (^a : (static member ToV3d : ^a -> V3d) (n))


let test (name : string) (there : V3d -> 'a) (back : 'a -> V3d) =
    let rand = RandomSystem()

    let mutable sum = 0.0
    let mutable h = 0.0
    let mutable l = System.Double.PositiveInfinity
    let mutable count = 0
    for i in 1 .. 100000000 do
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


let run() = 
    //16bit
    //  min: 1.131e-004°
    //  max: 6.348e-001°
    //  avg: 2.651e-001°
    //32bit
    //  min: 0.000e+000°
    //  max: 2.465e-003°
    //  avg: 1.028e-003°
    test "16bit" normal16 Normal.toV3d
    test "32bit" normal32 Normal.toV3d
    test "64bit" normal64 Normal.toV3d