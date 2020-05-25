namespace LsupEval

[<AutoOpen>]
module F =
    
    let toRegEx (lsuPattern:string) =
        let pattern = lsuPattern.Replace("*",".*")
        new System.Text.RegularExpressions.Regex(pattern)

