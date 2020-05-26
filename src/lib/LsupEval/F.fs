namespace LsupEval

[<AutoOpen>]
module F =
    open System.Collections.Generic
    open System.Text.RegularExpressions

    let toRegEx (lsuPattern:string) =
        let pattern = lsuPattern.Replace("*",".*")
        new System.Text.RegularExpressions.Regex(pattern)


    let dictToMap (dic : System.Collections.Generic.IDictionary<_,_>) = 
        dic 
        |> Seq.map (|KeyValue|)  
        |> Map.ofSeq

    let mapToDict map =
        map 
        |> Map.toSeq
        |> dict

    let toKeyValuePair (key,value) =
        new KeyValuePair<_,_>(key,value)

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

