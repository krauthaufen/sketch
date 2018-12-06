#load "load.fsx"


open System
open System.Xml
open System.Collections.Generic
open Aardvark.Base

let parseRmdXml (fn : string) =
    let doc = XmlDocument();
    doc.Load fn

    let nameToLabel =
        let a = List<_>()
        for n in doc.GetElementsByTagName "PointLabel" do
            let id = n.Attributes.["Id"]
            let name = n.["Name"]
            a.Add (name.InnerText, id.Value)
        a |> Seq.toList |> Map.ofList

    let labelToVertex =
        let a = List<_>()
        for n in doc.GetElementsByTagName "Point" do
            let vid = n.["VertexId"]
            let labid = n.["PointLabelId"]
            a.Add (labid.InnerText, vid.InnerText)
        a |> Seq.toList |> Map.ofList

    let vertexToCoord =
        let a = List<_>()
        for n in doc.GetElementsByTagName "Vertex" do
            let vid = n.Attributes.["Id"].Value

            let x = n.["East"].InnerText
            let y = n.["North"].InnerText
            let z = n.["Elevation"].InnerText
            let ostr = [x;y;z] |> String.concat " "


            a.Add (vid, ostr)
        a |> Seq.toList |> Map.ofList
        
    fun (name : string) ->
        vertexToCoord.[labelToVertex.[nameToLabel.[name]]]

let coord = parseRmdXml @"D:\bla2\xml\Tachymetrie_Beiglboeck-2015.xml"

let c (i : int) = coord (string i)

let cs = 
    [ 1002 .. 1071 ] |> List.map (fun i -> i,c i)