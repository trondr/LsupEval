namespace LsupEval

module Rules =
    open System    
    open Logging
    open LsupEval.Bios
    open LsupEval.Cpu
    open LsupEval.OperatingSystem
    open LsupEval.Driver
    open LsupEval.EmbeddedController
    open LsupEval.File
    open LsupEval.Registry
    open LsupEval.PnPId
    open LsupEval.ExternalDetection

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
        |OsLang of OsLang
        |Driver of DriverElement
        |EmbeddedControllerVersion of EmbeddedControllerVersionElement
        |FileExists of FileExistsElement
        |FileVersion of FileVersionPattern
        |RegistryKeyExists of RegistryKeyExistPattern
        |PnPId of PnPIdPattern
        |RegistryKeyValue of RegistryKeyValuePattern
        |ExternalDetection of ExternalDetection
            
    type SystemInformation =
        {
            BiosVersion:string
            CpuAddressWidth:Cpu.CpuAddressWidth
            Os:string
            OsLang:string
            Drivers: DriverInfo[]
            EmbeddedControllerVersion:string
            PnPIds:string[]
        }

    let getCurrentSystemInformation() =
        result{
            let! biosVersion = Bios.getCurrentBiosVersion()        
            let! cupAddressWidth = Cpu.getCurrentCpuAddressWidth()
            let! os = OperatingSystem.getCurrentOperatingSystem()
            let! drivers = Driver.getCurrentDrivers()
            let! embeddedControllerVersion = getCurrentEmbeddedControllerVersion()
            let! osLang = LsupEval.OperatingSystem.getCurrentOsLanguage()
            let pnpIds = LsupEval.PnPId.getCurrentPnpIds drivers
            return 
                {
                    BiosVersion = biosVersion
                    CpuAddressWidth = cupAddressWidth
                    Os = os
                    OsLang=osLang
                    Drivers = drivers
                    EmbeddedControllerVersion = embeddedControllerVersion
                    PnPIds = pnpIds
                }
        }

    let rec evaluateApplicabilityRule (logger:Common.Logging.ILog) systemInfo workingDirectory applicabilityRule =
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
            let isMatch = al |> Seq.forall (evaluateApplicabilityRule logger systemInfo workingDirectory)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating And rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |Or al -> 
            let isMatch = al |> Seq.exists (evaluateApplicabilityRule logger systemInfo workingDirectory)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating Or rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |Not al -> 
            let isMatch = not (evaluateApplicabilityRule logger systemInfo workingDirectory al)
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
        |OsLang osLang ->
            let isMatch = (isOsLanguageMatch logger osLang.Languages systemInfo.OsLang)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating OperatingSystem language rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
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
        |FileVersion fileversionPattern ->
            let fileVersion = LsupEval.File.getFileVersionFromFileVersionPattern fileversionPattern
            let isMatch = (LsupEval.File.isFileVersionMatch logger fileversionPattern fileVersion)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating FileVersion rule: '%A' with '%A'. Return: %b" applicabilityRule fileVersion isMatch))|>ignore))
            isMatch
        |RegistryKeyExists registryKeyExistsPattern ->
            let registryKeyStatuses = LsupEval.Registry.getRegistryKeyStatusesFromRegistryKeyExistPattern registryKeyExistsPattern
            let isMatch = (LsupEval.Registry.isRegistryKeyMatch logger registryKeyExistsPattern registryKeyStatuses)
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating registry exists rule: '%A' with '%A'. Return: %b" applicabilityRule registryKeyStatuses isMatch))|>ignore))
            isMatch
        |PnPId pnPIdPattern ->                        
            let isMatch = LsupEval.PnPId.isPnPIdMatch logger pnPIdPattern systemInfo.PnPIds
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating PnPId rule: '%A'. Return: %b" applicabilityRule isMatch))|>ignore))
            isMatch
        |RegistryKeyValue registryKeyValuePattern ->
            let registryKeyValueStatuses = LsupEval.Registry.getRegistryKeyValueStatuses registryKeyValuePattern
            let isMatch = LsupEval.Registry.isRegistryKeyValueMatch logger registryKeyValuePattern registryKeyValueStatuses
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating registry key value rule: '%A' with '%A'. Return: %b" applicabilityRule registryKeyValueStatuses isMatch))|>ignore))
            isMatch
        |ExternalDetection externalDetection ->
            //1. Execute command line
            let processExitData = LsupEval.ExternalDetection.executeCommandLine externalDetection.CommandLine workingDirectory
            logger.Info(new Msg(fun m -> m.Invoke( (sprintf "External detection executed and exited with:\n'%A'" processExitData))|>ignore))

            //2. Check return codes
            let isMatch = LsupEval.ExternalDetection.returnCodeIsMatch processExitData.ExitCode externalDetection.ReturnCodes
            logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Evaluating external detection rule: '%A' with '%A'. Return: %b" applicabilityRule externalDetection isMatch))|>ignore))
            isMatch




            