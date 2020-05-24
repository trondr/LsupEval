namespace LsupEval

module Bios =
    open System.Management
    open LsupEval.Logging
    open LsupEval.WmiHelper
    let logger = getLoggerByName "Bios"

    type Bios = {Versions:string[]}

    let toRegEx (lsuPattern:string) =
        let pattern = lsuPattern.Replace("*",".*")
        new System.Text.RegularExpressions.Regex(pattern)

    let getCurrentBiosVersion () =
        result{
            let! objectValue = getWmiPropertyValue "Win32_BIOS" "SMBIOSBIOSVersion"
            return  objectValue :?> string
        }

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

