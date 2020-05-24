namespace LsupEval

module Bios =
    open System.Management
    open LsupEval.Logging
    let logger = getLoggerByName "Bios"

    type Bios = {Versions:string[]}

    let toRegEx (lsuPattern:string) =
        let pattern = lsuPattern.Replace("*",".*")
        new System.Text.RegularExpressions.Regex(pattern)

    let getCurrentBiosVersion () =
        try
            use searcher = new ManagementObjectSearcher("SELECT SMBIOSBIOSVersion FROM WIN32_BIOS")
            use list = searcher.Get()
            let biosVersion = 
                list
                |>Seq.cast
                |>Seq.map(fun (x:ManagementObject) -> x.["SMBIOSBIOSVersion"])
                |>Seq.head
            Result.Ok (biosVersion :?> string)
        with
        |ex -> 
            Result.Error (toException ("Failed to get BIOS version.") (Some ex))

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

