namespace LsupEval

[<AutoOpen>]
module XmlHelper=
    
    open System.Xml.Linq

    let xn s = 
        XName.Get(s)

