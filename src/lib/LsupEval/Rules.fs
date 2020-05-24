namespace LsupEval

module Rules =
    open System
    open Logging

    type Bios = {Versions:string[]}

    type ApplicabilityRule =
        |True
        |False
        |And of ApplicabilityRule seq
        |Or of ApplicabilityRule seq
        |Not of ApplicabilityRule
        |Bios of Bios
    
    type SystemInformation =
        {
            BiosVersion:string
        }

    let getCurrentSystemInformation() =
        {
            BiosVersion = "Not implemented"
        }

    let toRegEx (lsuPattern:string) =
        let pattern = lsuPattern.Replace("*",".*")
        new System.Text.RegularExpressions.Regex(pattern)

    let isBiosMatch (logger:Common.Logging.ILog) (bios:Bios) biosVersion =        
        let v = 
            bios.Versions 
            |> Seq.filter (fun s -> 
                    let isMatch = (toRegEx s).IsMatch(biosVersion)
                    logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing bios version: '%s' with bios level '%s'. Return: %b" biosVersion s isMatch))|>ignore))
                    isMatch
                ) |>Seq.tryHead
        match v with
        |Some -> true
        |None -> false

    let rec evaluateApplicabilityRule (logger:Common.Logging.ILog) systemInfo applicabilityRule =
        match applicabilityRule with
        |True -> 
            let isMatch = true
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating True rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |False -> 
            let isMatch = false
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating False rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |And al -> 
            let isMatch = al |> Seq.forall (evaluateApplicabilityRule logger systemInfo)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating And rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |Or al -> 
            let isMatch = al |> Seq.exists (evaluateApplicabilityRule logger systemInfo)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating Or rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |Not al -> 
            let isMatch = not (evaluateApplicabilityRule logger systemInfo al)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating Not rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |Bios bios ->
            let isMatch = (isBiosMatch logger bios systemInfo.BiosVersion)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating BIOS rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
            