﻿namespace LsupEval

module Bios =
    open System.Management
    open LsupEval.Logging
    open LsupEval.WmiHelper
    let logger = getLoggerByName "LsupEval.Bios"

    type Bios = {Versions:string[]}

    let getCurrentBiosVersion () =
        result{
            let! valueObject = getWmiPropertyValue "Win32_BIOS" "SMBIOSBIOSVersion"
            let! valueString = objectToString valueObject
            return  valueString
        }

    let isBiosMatch (logger:Common.Logging.ILog) (bios:Bios) biosVersion =
        let v = 
            bios.Versions 
            |> Seq.filter (fun s -> 
                    let isMatch = (toRegEx s).IsMatch(biosVersion)
                    if(logger.IsDebugEnabled) then logger.Debug(sprintf "Comparing bios version: '%s' with bios level '%s'. Return: %b" biosVersion s isMatch)
                    isMatch
                ) |>Seq.tryHead
        match v with
        |Some _ -> true
        |None -> false

