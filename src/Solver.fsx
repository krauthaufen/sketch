#r "netstandard"
#r "../packages/Aardvark.Base.TypeProviders/lib/net45/Aardvark.Base.TypeProviders.dll"
#r "../packages/Aardvark.Base.Essentials/lib/netstandard2.0/Aardvark.Base.Essentials.dll"
#r "../packages/Aardvark.Base.Telemetry/lib/netstandard2.0/Aardvark.Base.Telemetry.dll"
#r "../packages/Aardvark.Base/lib/netstandard2.0/Aardvark.Base.dll"
#r "../packages/FSharp.Data.Adaptive/lib/netstandard2.0/FSharp.Data.Adaptive.dll"
#r "../packages/Aardvark.Base.FSharp/lib/netstandard2.0/Aardvark.Base.FSharp.dll"

open Aardvark.Base
open System.Runtime.CompilerServices

[<AbstractClass; Sealed; Extension>]
type Solver =
    
    [<Extension>]
    static member Multiply(m0 : Matrix<ComplexD>, m1 : Matrix<ComplexD>) =
        if m0.SX <> m1.SY then raise <| System.ArgumentException("m0.SX != m1.SY")

        let result = Matrix<ComplexD>(m1.SX, m0.SY)

        let data = result.Data
        let data0 = m0.Data
        let data1 = m1.Data

        let mutable i = result.FirstIndex
        let yj = result.JY
        let my0 = m0.DY

        let xs = result.DSX
        let mf1 = m1.FirstIndex
        let xj = result.JX
        let mx1 = m1.DX

        let ds0 = m0.DSX
        let d0 = m0.DX
        let d1 = m1.DY

        let mutable ye = i + result.DSY
        let mutable f0 = m0.FirstIndex
        let mutable e0 = f0 + ds0
        while i <> ye do

            let mutable xe = i + xs
            let mutable f1 = mf1

            while i <> xe do
                let mutable dot = ComplexD.Zero

                let mutable i0 = f0
                let mutable i1 = f1
                while i0 <> e0 do
                    dot <- dot + data0.[int i0] * data1.[int i1]
                    
                    i0 <- i0 + d0
                    i1 <- i1 + d1
                data.[int i] <- dot

                i <- i + xj
                f1 <- f1 + mx1

            i <- i + yj
            f0 <- f0 + my0
            e0 <- e0 + my0

        result

    [<Extension>]
    static member Multiply(mat : Matrix<ComplexD>, vec : Vector<ComplexD>) =
        if mat.SX <> vec.Size then raise <| System.ArgumentException("m0.SX != m1.SY")

        let result = Array.zeroCreate (int mat.Dim.Y)

        let data0 = mat.Data
        let data1 = vec.Data
        let my0 = mat.DY
        let d1 = vec.D
        let mf1 = vec.FirstIndex
        let ds0 = mat.DSX
        let d0 = mat.DX


        let mutable ri = 0L
        let mutable ye = mat.FirstIndex + mat.DSY
        let mutable f0 = mat.FirstIndex
        let mutable e0 = f0 + ds0
        while f0 <> ye do
            
            let mutable dot = ComplexD.Zero
            let mutable i0 = f0
            let mutable i1 = mf1
            while i0 <> e0 do
                dot <- dot + data0.[int i0] * data1.[int i1]
                i0 <- i0 + d0
                i1 <- i1 + d1
            result.[int ri] <- dot

            f0 <- f0 + my0
            e0 <- e0 + my0
            ri <- ri + 1L

        Vector.Create result

    /// solves `a*x = b` with some known `xi`. Note that for over-determined systems this will
    /// return a least-squares solution.
    [<Extension>]
    static member Solve (a : Matrix<ComplexD>, b : Vector<ComplexD>, ?known : Map<int, ComplexD>) =
        let known = defaultArg known Map.empty
        if Map.isEmpty known then
            if a.SX = a.SY then
                let a = a.Copy()
                let perm = a.LuFactorize()
                a.LuSolve(perm, b)
            else
                let a1 = a.Transposed.Multiply(a)
                let b1 = a.Transposed.Multiply(b)
                let perm = a1.LuFactorize()
                a1.LuSolve(perm, b1)
        else
            let unknown = Seq.init (int a.SX) id |> Seq.filter (fun i -> not (Map.containsKey i known)) |> Seq.toArray

            let res = Matrix<ComplexD>(unknown.LongLength, a.SY)
            let mutable bb = Vector<ComplexD>(a.SY)

            for ri in 0 .. int a.SY - 1 do
                let mutable mRow = a.GetRow(ri)
                let mutable resRow = res.GetRow(ri)
                let mutable i = 0
                for ci in unknown do
                    resRow.[i] <- mRow.[ci]
                    i <- i + 1
            
                let mutable rhs = b.[ri]
                for KeyValue(id, value) in known do
                    rhs <- rhs - value * mRow.[id]

                bb.[ri] <- rhs


            let a1 = res.Transposed.Multiply(res)
            let b1 = res.Transposed.Multiply bb

            let perm = a1.LuFactorize()
            let xx = a1.LuSolve(perm, b1)

            let mutable x = Vector<ComplexD>(int a.SX)
            for KeyValue(i, v) in known do
                x.[i] <- v

            let mutable i = 0
            for id in unknown do
                x.[id] <- xx.[i]
                i <- i + 1

            x

    /// solves `a*x = b` with some known `xi`. Note that for over-determined systems this will
    /// return a least-squares solution.
    [<Extension>]
    static member Solve (a : Matrix<float>, b : Vector<float>, ?known : Map<int, float>) =
        let known = defaultArg known Map.empty
        if Map.isEmpty known then
            if a.SX = a.SY then
                let a = a.Copy()
                let perm = a.LuFactorize()
                a.LuSolve(perm, b)
            else
                let a1 = a.Transposed.Multiply(a)
                let b1 = a.Transposed.Multiply(b)
                let perm = a1.LuFactorize()
                a1.LuSolve(perm, b1)
        else
            let unknown = Seq.init (int a.SX) id |> Seq.filter (fun i -> not (Map.containsKey i known)) |> Seq.toArray

            let res = Matrix<float>(unknown.LongLength, a.SY)
            let mutable bb = Vector<float>(a.SY)

            for ri in 0 .. int a.SY - 1 do
                let mutable mRow = a.GetRow(ri)
                let mutable resRow = res.GetRow(ri)
                let mutable i = 0
                for ci in unknown do
                    resRow.[i] <- mRow.[ci]
                    i <- i + 1
            
                let mutable rhs = b.[ri]
                for KeyValue(id, value) in known do
                    rhs <- rhs - value * mRow.[id]

                bb.[ri] <- rhs


            let a1 = res.Transposed.Multiply(res)
            let b1 = res.Transposed.Multiply bb

            let perm = a1.LuFactorize()
            let xx = a1.LuSolve(perm, b1)

            let mutable x = Vector<float>(int a.SX)
            for KeyValue(i, v) in known do
                x.[i] <- v

            let mutable i = 0
            for id in unknown do
                x.[id] <- xx.[i]
                i <- i + 1

            x

module SolverTest = 
    let printMatrix (m : Matrix<'a>) =
        let names = Array.init (int (max m.SX m.SY)) string
   
        let mutable entries = Matrix<string>(m.Size)
        entries.Info.ForeachXYIndex(m.Info, fun (x : int64) (y : int64) (i : int64) (mi : int64) ->
            entries.[i] <- sprintf "%A" m.[mi]
        ) |> ignore

        let longestName = names |> Seq.map String.length |> Seq.max
        let longestEntry = entries.Data |> Seq.map String.length |> Seq.max

        let width = max longestEntry longestName
        let inline padl (str : string) =
            if str.Length < width then 
                let m = (width - str.Length)
                str + System.String(' ', m)
            else
                str

        let names = names |> Array.map padl
        let entries = entries.Map padl

        let ws = padl ""
        let minuses = System.String('-', width)
        let header = names |> Array.take (int m.SX) |> String.concat " | " |> sprintf "| %s | %s |" ws
        let sep = Seq.init (int m.SX) (fun _ -> sprintf ":%s " minuses) |> String.concat "|"
        Log.line "%s" header
        Log.line "| %s:|%s|" minuses sep
        for r in 0 .. int m.SY - 1 do
            let row = entries.GetRow(r)
            let data = Seq.init (int row.S) (fun i -> row.[i]) |> String.concat " | "
            Log.line "| %s | %s |" names.[r] data

    let test() =
        
        let m =
            Matrix(
                [|
                    1.0; 0.0; 0.0; 0.0; 
                    0.0; 1.0; 0.0; 0.0; 
                    0.0; 0.0; 1.0; 0.0; 
                    0.0; 0.0; 0.0; 1.0; 
                    1.0; 0.0; 0.0; 1.0; 
                |],
                4L, 5L
            )

        let b =
            Vector([|1.0; 2.0; 3.0; 4.0; 5.0|])


        let known = Map.ofList [1, 2.0]


        let x = m.Map(ComplexD).Solve(b.Map(ComplexD), known |> Map.map (fun _ v -> ComplexD v))

        Log.start "A"
        printMatrix m
        Log.stop()

        
        Log.start "x"
        printMatrix (Matrix(x.Data, x.Size, 1L))
        Log.stop()

        
        Log.start "b"
        printMatrix (Matrix(b.Data, x.Size, 1L))
        Log.stop()


printfn "%A" System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription

SolverTest.test()
