﻿namespace LsupEval

module Rules =
    open System    
    open Logging
    open LsupEval.Bios
    open LsupEval.Cpu
    open LsupEval.OperatingSystem
    open LsupEval.Driver
    open LsupEval.EmbeddedController
    open LsupEval.File

    let logger = Logging.getLoggerByName "Rules"

    type ApplicabilityRule =
        |True
        |False
        |And of ApplicabilityRule seq
        |Or of ApplicabilityRule seq
        |Not of ApplicabilityRule
        |Bios of Bios
        |CpuAddressWidth of CpuAddressWidth
        |Os of Os
        |Driver of DriverElement
        |EmbeddedControllerVersion of EmbeddedControllerVersionElement
        |FileExists of FileExistsElement
    
    type SystemInformation =
        {
            BiosVersion:string
            CpuAddressWidth:Cpu.CpuAddressWidth
            Os:string
            Drivers: DriverInfo[]
            EmbeddedControllerVersion:string
        }

    let getCurrentSystemInformation() =
        result{
            let! biosVersion = Bios.getCurrentBiosVersion()        
            let! cupAddressWidth = Cpu.getCurrentCpuAddressWidth()
            let! os = OperatingSystem.getCurrentOperatingSystem()
            let! drivers = Driver.getCurrentDrivers()
            let! embeddedControllerVersion = getCurrentEmbeddedControllerVersion()
            return 
                {
                    BiosVersion = biosVersion
                    CpuAddressWidth = cupAddressWidth
                    Os = os
                    Drivers = drivers
                    EmbeddedControllerVersion = embeddedControllerVersion
                }
        }

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
        |CpuAddressWidth cpuAddressWidth ->
            let isMatch = (isCpuAddressWidthMatch logger cpuAddressWidth systemInfo.CpuAddressWidth)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating CpuAddressWidth rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |Os os ->
            let isMatch = (isOperatingSystemMatch logger os systemInfo.Os)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating OperatingSystem rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |Driver driver ->
            let fileDrivers = getFileDriversFromDriverPattern driver            
            let isMatch = (isDriverMatch logger driver (Array.append systemInfo.Drivers fileDrivers))
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating Driver rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |EmbeddedControllerVersion embeddedControllerVersion ->
            let isMatch = (isEmbeddedControllerVersionMatch logger embeddedControllerVersion systemInfo.EmbeddedControllerVersion)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating EmbeddedControllerVersion rule: '%A' with '%s'. Return: %b" applicabilityRule systemInfo.EmbeddedControllerVersion isMatch))|>ignore))
            isMatch
        |FileExists fileExistsElement ->
            let files = LsupEval.File.getFilesFromFileExistPattern fileExistsElement
            let isMatch = (LsupEval.File.isFileExistsMatch logger fileExistsElement files)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating FileExists rule: '%A' with '%A'. Return: %b" applicabilityRule files isMatch))|>ignore))
            isMatch



            