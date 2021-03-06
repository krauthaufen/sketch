#r "netstandard"
#r "../packages/Aardvark.Base.TypeProviders/lib/net45/Aardvark.Base.TypeProviders.dll"
#r "../packages/Aardvark.Base.Essentials/lib/netstandard2.0/Aardvark.Base.Essentials.dll"
#r "../packages/Aardvark.Base.Telemetry/lib/netstandard2.0/Aardvark.Base.Telemetry.dll"
#r "../packages/Aardvark.Base/lib/netstandard2.0/Aardvark.Base.dll"
#r "../packages/FSharp.Data.Adaptive/lib/netstandard2.0/FSharp.Data.Adaptive.dll"
#r "../packages/Aardvark.Base.FSharp/lib/netstandard2.0/Aardvark.Base.FSharp.dll"

open Aardvark.Base
open System.Runtime.CompilerServices

#nowarn "1337"
#nowarn "77"


type Complex = ComplexD
type Complex32 = ComplexF
type complex = Complex
type complex32 = Complex32


[<AutoOpen>]
module ComplexOperators =

    [<CompilerMessage("internal", 1337, IsHidden = true)>]
    type ComplexConverter =
        static member op_Explicit (a : int8) = ComplexD (float a)
        static member op_Explicit (a : uint8) = ComplexD (float a)
        static member op_Explicit (a : int16) = ComplexD (float a)
        static member op_Explicit (a : uint16) = ComplexD (float a)
        static member op_Explicit (a : int) = ComplexD (float a)
        static member op_Explicit (a : uint32) = ComplexD (float a)
        static member op_Explicit (a : int64) = ComplexD (float a)
        static member op_Explicit (a : uint64) = ComplexD (float a)
        static member op_Explicit (a : float32) = ComplexD (float a)
        static member op_Explicit (a : ComplexF) = ComplexD(float a.Real, float a.Imag)
        
        static member op_Explicit (a : int8) = ComplexF (float32 a)
        static member op_Explicit (a : uint8) = ComplexF (float32 a)
        static member op_Explicit (a : int16) = ComplexF (float32 a)
        static member op_Explicit (a : uint16) = ComplexF (float32 a)
        static member op_Explicit (a : int) = ComplexF (float32 a)
        static member op_Explicit (a : uint32) = ComplexF (float32 a)
        static member op_Explicit (a : int64) = ComplexF (float32 a)
        static member op_Explicit (a : uint64) = ComplexF (float32 a)
        static member op_Explicit (a : float) = ComplexF (float32 a)
        static member op_Explicit (a : ComplexD) = ComplexF(float32 a.Real, float32 a.Imag)


    let inline private complexAux (a : ^a) (b : ^b) : ^c =  
        ((^a or ^b or ^c) : (static member op_Explicit : ^b -> ^c) (b))

    let inline complex a : complex = complexAux Unchecked.defaultof<ComplexConverter> a
    let inline complex32 a : complex32 = complexAux Unchecked.defaultof<ComplexConverter> a


module private MatrixSolver =


    let inline multiply (m0 : Matrix< ^a >) (m1 : Matrix< ^a >) =
        if m0.SX <> m1.SY then raise <| System.ArgumentException("m0.SX != m1.SY")

        let result = Matrix< ^a >(m1.SX, m0.SY)

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
                let mutable dot = LanguagePrimitives.GenericZero< ^a >

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
        
    let inline multiplyVec (mat : Matrix< ^a >) (vec : Vector< ^a >) =
        if mat.SX <> vec.Size then raise <| System.ArgumentException("mat.SX != vec.Size")

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
            
            let mutable dot = LanguagePrimitives.GenericZero< ^a >
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

    let inline addVec (v0 : Vector< ^a >) (v1 : Vector< ^a >) =
        if v0.Size <> v1.Size then failwith "wrong vector sizes"

        let res = Array.zeroCreate (int v0.Size)
        let mutable i0 = v0.Origin
        let mutable i1 = v1.Origin
        let e0 = i0 + v0.Size * v0.Delta
        let d0 = v0.Delta
        let d1 = v1.Delta
        let v0 = v0.Data
        let v1 = v1.Data
        let mutable oi = 0


        while i0 <> e0 do
            res.[oi] <- v0.[int i0] + v1.[int i1]
            i0 <- i0 + d0
            i1 <- i1 + d1
            oi <- oi + 1

        Vector.Create res
 
    let inline subVec (v0 : Vector< ^a >) (v1 : Vector< ^a >) =
        if v0.Size <> v1.Size then failwith "wrong vector sizes"

        let res = Array.zeroCreate (int v0.Size)
        let mutable i0 = v0.Origin
        let mutable i1 = v1.Origin
        let e0 = i0 + v0.Size * v0.Delta
        let d0 = v0.Delta
        let d1 = v1.Delta
        let v0 = v0.Data
        let v1 = v1.Data
        let mutable oi = 0


        while i0 <> e0 do
            res.[oi] <- v0.[int i0] - v1.[int i1]
            i0 <- i0 + d0
            i1 <- i1 + d1
            oi <- oi + 1

        Vector.Create res



    [<AutoOpen>]
    module Helpers = 

        let inline luFactorizeAux (dummy : ^d) (a : Matrix< ^a >) =
            ((^d or ^a or ^b) : (static member LuFactorize : Matrix< ^a > -> int[]) (a))

        let inline luSolveAux (dummy : ^d) (a : Matrix< ^a >) (perm : int[]) (b : Vector< ^c >) =
            ((^d or ^a) : (static member LuSolve : Matrix< ^a > * int[] * Vector< ^c > -> Vector< ^c > ) (a, perm, b))
            
        let inline luFactorize a = luFactorizeAux Unchecked.defaultof<NumericExtensions> a
        let inline luSolve a b c = luSolveAux Unchecked.defaultof<NumericExtensions> a b c

    let inline solve (copy : bool) (mat : Matrix< ^a >) (vec : Vector< ^a >) (known : Map<int, ^a >) =
        if vec.Size <> mat.SY then failwithf "vec.Size <> mat.SY (%d vs %d)" vec.Size mat.SY

        let known = known |> Map.filter (fun i _ -> i >= 0 && i < int mat.SX)
        if Map.isEmpty known then
            if mat.SX > mat.SY then
                failwith "cannot solve underconstrained system"

            elif mat.SX = mat.SY then
                let a = if copy then mat.Copy() else mat
                let perm = luFactorize a
                luSolve a perm vec
            else
                let a1 = multiply mat.Transposed mat
                let b1 = multiplyVec mat.Transposed vec
                let perm = luFactorize a1
                luSolve a1 perm b1
        else
            let unknown = Seq.init (int mat.SX) id |> Seq.filter (fun i -> not (Map.containsKey i known)) |> Seq.toArray

            if unknown.LongLength > mat.SY then failwith "cannot solve underconstrained system"

            let res = Matrix< ^a >(unknown.LongLength, mat.SY)
            let mutable bb = Vector< ^a >(mat.SY)

            for ri in 0 .. int mat.SY - 1 do
                let mutable mRow = mat.GetRow(ri)
                let mutable resRow = res.GetRow(ri)
                let mutable i = 0
                for ci in unknown do
                    resRow.[i] <- mRow.[ci]
                    i <- i + 1
            
                let mutable rhs = vec.[ri]
                for KeyValue(id, value) in known do
                    rhs <- rhs - value * mRow.[id]

                bb.[ri] <- rhs

            let xx = 
                if res.SX = res.SY then
                    let perm = luFactorize res
                    luSolve res perm bb
                else
                    let a1 = multiply res.Transposed res
                    let b1 = multiplyVec res.Transposed bb
                    let perm = luFactorize a1
                    luSolve a1 perm b1

            let mutable x = Vector< ^a >(int mat.SX)
            for KeyValue(i, v) in known do
                x.[i] <- v

            let mutable i = 0
            for id in unknown do
                x.[id] <- xx.[i]
                i <- i + 1

            x

    

[<AbstractClass; Sealed; Extension>]
type Solver private() =

    [<Extension>]
    static member Multiply(m0 : Matrix<ComplexD>, m1 : Matrix<ComplexD>) =
        MatrixSolver.multiply m0 m1

    [<Extension>]
    static member Multiply(mat : Matrix<ComplexD>, vec : Vector<ComplexD>) =
        MatrixSolver.multiplyVec mat vec

    [<Extension>]
    static member Multiply(m0 : Matrix<ComplexF>, m1 : Matrix<ComplexF>) =
        MatrixSolver.multiply m0 m1

    [<Extension>]
    static member Multiply(mat : Matrix<ComplexF>, vec : Vector<ComplexF>) =
        MatrixSolver.multiplyVec mat vec
        
        
    [<Extension>]
    static member Add(v0 : Vector<ComplexD>, v1 : Vector<ComplexD>) =
        MatrixSolver.addVec v0 v1
        
    [<Extension>]
    static member Add(v0 : Vector<ComplexF>, v1 : Vector<ComplexF>) =
        MatrixSolver.addVec v0 v1

        
    [<Extension>]
    static member Subtract(v0 : Vector<ComplexD>, v1 : Vector<ComplexD>) =
        MatrixSolver.subVec v0 v1
        
    [<Extension>]
    static member Subtract(v0 : Vector<ComplexF>, v1 : Vector<ComplexF>) =
        MatrixSolver.subVec v0 v1
        

    [<Extension>]
    static member NormSquared(vec : Vector<ComplexF>) =
        let mutable i = vec.Origin
        let e = i + vec.Size * vec.Delta
        let d = vec.Delta
        let data = vec.Data
        let mutable sum = 0.0f
        while i <> e do
            sum <- sum + data.[int i].NormSquared
            i <- i + d

        sum

    [<Extension>]
    static member NormSquared(vec : Vector<ComplexD>) =
        let mutable i = vec.Origin
        let e = i + vec.Size * vec.Delta
        let d = vec.Delta
        let data = vec.Data
        let mutable sum = 0.0
        while i <> e do
            sum <- sum + data.[int i].NormSquared
            i <- i + d

        sum
        
    [<Extension>]
    static member Norm(vec : Vector<ComplexD>) =
        Solver.NormSquared(vec) |> sqrt
        
    [<Extension>]
    static member Norm(vec : Vector<ComplexF>) =
        Solver.NormSquared(vec) |> sqrt


    /// solves `mat*x = vec` with some known `xi`. Note that for over-determined systems this will
    /// return a least-squares solution.
    [<Extension>]
    static member Solve (mat : Matrix<ComplexD>, vec : Vector<ComplexD>, ?known : Map<int, ComplexD>) =
        MatrixSolver.solve true mat vec (defaultArg known Map.empty)
        


    /// solves `mat*x = vec` with some known `xi`. Note that for over-determined systems this will
    /// return a least-squares solution.
    [<Extension>]
    static member Solve (mat : Matrix<float>, vec : Vector<float>, ?known : Map<int, float>) =
        MatrixSolver.solve true mat vec (defaultArg known Map.empty)

    /// solves `mat*x = vec` with some known `xi`. Note that for over-determined systems this will
    /// return a least-squares solution.
    [<Extension>]
    static member Solve (mat : Matrix<float32>, vec : Vector<float32>, ?known : Map<int, float32>) =
        let mat1 = mat.Map(float)
        let vec1 = vec.Map(float)
        let known1 = defaultArg known Map.empty |> Map.map (fun _ v -> float v)
        let res = MatrixSolver.solve false mat1 vec1 known1
        res.Map(float32)

    /// solves `mat*x = vec` with some known `xi`. Note that for over-determined systems this will
    /// return a least-squares solution.
    [<Extension>]
    static member Solve (mat : Matrix<ComplexF>, vec : Vector<ComplexF>, ?known : Map<int, ComplexF>) =
        let mat1 = mat.Map(fun c -> ComplexD(float c.Real, float c.Imag))
        let vec1 = vec.Map(fun c -> ComplexD(float c.Real, float c.Imag))
        let known1 = defaultArg known Map.empty |> Map.map (fun _ c -> ComplexD(float c.Real, float c.Imag))
        let res = MatrixSolver.solve false mat1 vec1 known1
        res.Map(fun c -> ComplexF(float32 c.Real, float32 c.Imag))

[<AbstractClass; Sealed; Extension>]
type MatrixExtensions private ()=

    static let rx = System.Text.RegularExpressions.Regex @"^(-)?0\.[0]+((E|e)-?[0]+)?$"

    static let printMatrix (print : 'a -> string) (m : IMatrix<'a>) =
        let names = Array.init (int (max m.Dim.X m.Dim.Y)) string
   
        let mutable entries = Matrix<string>(m.Dim)
        entries.SetByCoord (fun (v : V2l) ->
            print m.[v]
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

        let entries = entries.Map padl
        for r in 0 .. int m.Dim.Y - 1 do
            let row = entries.GetRow(r)
            let data = Seq.init (int row.S) (fun i -> row.[i]) |> String.concat " "
            Log.line "%s" data

    [<Extension>]
    static member Print(this : IMatrix<'a>) =
        printMatrix (sprintf "%A") this
     
    [<Extension>]
    static member Print(this : IVector<'a>) =
        let m = Matrix<'a>(V2l(1L, this.Dim))
        m.SetByCoord(fun (v : V2l) -> this.[v.Y]).Print()

        
    [<Extension>]
    static member Print(this : IMatrix<float>, fmt : string) =   
        this |> printMatrix (fun a -> a.ToString fmt)
        
    [<Extension>]
    static member Print(this : IVector<float>, fmt : string) =
        let m = Matrix<float>(V2l(1L, this.Dim))
        m.SetByCoord(fun (v : V2l) -> this.[v.Y]).Print(fmt)
       
    [<Extension>]
    static member Print(this : IMatrix<complex>, fmt : string) =  
    
        let printComplex (c : complex) =
            let real = c.Real.ToString(fmt)
            let imag = c.Imag.ToString(fmt)

            let rz = rx.IsMatch real
            let iz = rx.IsMatch imag

            if rz && iz then real
            elif rz && not iz then sprintf "%si" imag
            elif iz && not rz then real
            else sprintf "%s + %si" real imag


        this |> printMatrix printComplex
        
    [<Extension>]
    static member Print(this : IVector<ComplexD>, fmt : string) =
        let m = Matrix<ComplexD>(V2l(1L, this.Dim))
        m.SetByCoord(fun (v : V2l) -> this.[v.Y]).Print(fmt)

let test() =
    let m =
        Matrix(
            [|
                1.0; 0.0; 1.0; 0.0; 
                0.0; 1.0; 0.0; 0.0; 
                0.0; 0.0; 1.0; 0.0; 
                0.0; 0.0; 1.0; 1.0; 
            |],
            4L, 4L
        )

    let b =
        Vector([|4.0; 2.0; 3.0; 7.0|])


    let known = Map.ofList [2, 3.0]


    let x = m.Map(ComplexD).Solve(b.Map(ComplexD), known |> Map.map (fun _ v -> ComplexD v))

    Log.start "A"
    m.Print("0.00")
    Log.stop()

    Log.start "x"
    x.Print("0.00")
    Log.stop()

    Log.start "b"
    b.Print("0.00")
    Log.stop()

    let error = m.Map(ComplexD).Multiply(x).Subtract(b.Map(ComplexD)).NormSquared()
    Log.line "error: %A" error
    ()



test()
