namespace LsupEval

[<AutoOpen>]
module F =
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open System.Management.Automation
    open System.Collections.ObjectModel

    let toRegEx (lsuPattern:string) =
        let pattern = lsuPattern.Replace("*",".*").Replace(@"\",@"\\")
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

    let toOption value = 
        match value with
        |null -> None
        |_ -> Some value

    let runPowerShell (action:PowerShell->Collection<PSObject>) =
        use powershell = PowerShell.Create(RunspaceMode.NewRunspace)
        action(powershell)

    let toBoolean option =
        match option with
        |Some v -> true
        |None -> false
        
    let ifTrueThen success =
        function
        |true -> Some success
        |false -> None

    let (|NullOrEmpty|_|) =
        String.IsNullOrWhiteSpace 
        >> ifTrueThen NullOrEmpty
