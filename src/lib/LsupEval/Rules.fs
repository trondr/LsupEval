namespace LsupEval

module Rules =
    open System    
    open Logging
    open LsupEval.Bios
    open LsupEval.Windows
    open LsupEval.Cpu
    open LsupEval.OperatingSystem
    open LsupEval.Driver
    open LsupEval.EmbeddedController
    open LsupEval.File
    open LsupEval.Registry
    open LsupEval.PnPId
    open LsupEval.ExternalDetection
    open LsupEval.Coreq
    open LsupEval.LsupData
    open MBrace.FsPickler
    open System.Runtime.InteropServices

    let logger = Logging.getLoggerByName "Rules"

    ///Get system information. Note! This function takes approx 45 seconds to run. Use the cached version to increase performance: getCurrentSystemInformation'()
    let getCurrentSystemInformation() =
        result{
            let! biosVersion = Bios.getCurrentBiosVersion()        
            let! cupAddressWidth = Cpu.getCurrentCpuAddressWidth()
            let! os = OperatingSystem.getCurrentOperatingSystem()
            let! drivers = Driver.getCurrentDrivers()
            let! embeddedControllerVersion = getCurrentEmbeddedControllerVersion()
            let! osLang = LsupEval.OperatingSystem.getCurrentOsLanguage()
            let pnpIds = LsupEval.PnPId.getCurrentPnpIds drivers
            let windowsBuildVersion = LsupEval.Windows.getWindowsBuildVersion()
            return 
                {
                    BiosVersion = biosVersion
                    CpuAddressWidth = cupAddressWidth
                    Os = os
                    OsLang=osLang
                    Drivers = drivers
                    EmbeddedControllerVersion = embeddedControllerVersion
                    PnPIds = pnpIds
                    WindowsBuildVersion = windowsBuildVersion
                }
        }

    [<DllImport("kernel32.dll")>]
    extern uint64 GetTickCount64();

    ///Get time span since system was last rebooted
    let getSystemUptime() =
        let millisecondsSinceLastRestart = int64(GetTickCount64());
        let ticksSinceLastRestart = millisecondsSinceLastRestart * TimeSpan.TicksPerMillisecond;
        new TimeSpan(ticksSinceLastRestart);
    
    ///Get time span since file was last modified.
    let getFileUptime fileName = 
        if(fileExists fileName) then
            System.DateTime.Now - System.IO.File.GetLastWriteTime(fileName)
        else
            new TimeSpan(0L);

    ///Configure system information cache file. Clear cache after each system restart.
    let internal configureSystemInformationCacheFile () =
        let cacheFile = System.IO.Path.Combine(System.Environment.GetEnvironmentVariable("TEMP"),"Lsup-SystemInformation-73cba29c-207e-4527-9cdd-c7605f7877af.xml")        
        let cacheFileUptime = getFileUptime(cacheFile)
        let systemUptime = getSystemUptime()
        if(cacheFileUptime > systemUptime && (fileExists cacheFile)) then
            deleteFile cacheFile
        cacheFile

    ///Load cached information or call orginal information loader on cache miss.
    let getAndUpdateInformationCache<'T> (cacheFile:string) (onCacheMiss : unit->Result<'T,Exception>) : Result<'T,Exception> =
        let pickle = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
        let t =
            try
                use sr = new System.IO.StreamReader(cacheFile)
                logger.Info(sprintf "Loading cached information...")
                Result.Ok (pickle.Deserialize<'T>(sr))
            with
            |ex -> 
                logger.Info(sprintf "Unable to load cached information due to %s. Calling expensive information loader." ex.Message)
                onCacheMiss()
        
        match(t)with
        |Result.Ok tr -> 
            logger.Info(sprintf "Saving cached information...")
            use sw = new System.IO.StreamWriter(cacheFile)
            pickle.Serialize<'T>(sw,tr)
        |Result.Error ex ->
            logger.Info(sprintf "Unable to load cached information or get orginal information due to %s." ex.Message)
            ()
        t
    
    ///Get the current system information, if a cached version exists use it.
    let getCurrentSystemInformation' () =
        result{
            let cachePath = configureSystemInformationCacheFile ()                        
            let! systemInformation =
                getAndUpdateInformationCache cachePath getCurrentSystemInformation                                    
            return systemInformation
        }

    let rec evaluateApplicabilityRule (logger:Common.Logging.ILog) systemInfo workingDirectory (lsuPackages: LsuPackage[] option) applicabilityRule =
        match applicabilityRule with
        |True -> 
            let isMatch = true
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating True rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |False -> 
            let isMatch = false
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating False rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |And al -> 
            let isMatch = al |> Seq.forall (evaluateApplicabilityRule logger systemInfo workingDirectory lsuPackages)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating And rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |Or al -> 
            let isMatch = al |> Seq.exists (evaluateApplicabilityRule logger systemInfo workingDirectory lsuPackages)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating Or rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |Not al -> 
            let isMatch = not (evaluateApplicabilityRule logger systemInfo workingDirectory lsuPackages al)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating Not rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |Bios bios ->
            let isMatch = (isBiosMatch logger bios systemInfo.BiosVersion)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating BIOS rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |WindowsBuildVersion windowsBuildVersion ->
            let isMatch = (isWindowsBuildVersionMatch logger windowsBuildVersion systemInfo.WindowsBuildVersion)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating WindowsBuildVersion rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |CpuAddressWidth cpuAddressWidth ->
            let isMatch = (isCpuAddressWidthMatch logger cpuAddressWidth systemInfo.CpuAddressWidth)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating CpuAddressWidth rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |Os os ->
            let isMatch = (isOperatingSystemMatch logger os systemInfo.Os)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating OperatingSystem rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |OsLang osLang ->
            let isMatch = (isOsLanguageMatch logger osLang.Languages systemInfo.OsLang)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating OperatingSystem language rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |Driver driver ->
            let fileDrivers = getFileDriversFromDriverPattern driver            
            let isMatch = (isDriverMatch logger driver (Array.append systemInfo.Drivers fileDrivers))
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating Driver rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |EmbeddedControllerVersion embeddedControllerVersion ->
            let isMatch = (isEmbeddedControllerVersionMatch logger embeddedControllerVersion systemInfo.EmbeddedControllerVersion)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating EmbeddedControllerVersion rule: '%A' with '%s'. Return: %b" applicabilityRule systemInfo.EmbeddedControllerVersion isMatch)
            isMatch
        |FileExists fileExistsElement ->
            let files = LsupEval.File.getFilesFromFileExistPattern fileExistsElement
            let isMatch = (LsupEval.File.isFileExistsMatch logger fileExistsElement files)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating FileExists rule: '%A' with '%A'. Return: %b" applicabilityRule files isMatch)
            isMatch
        |FileVersion fileversionPattern ->
            let filePath = expandEvironmentVariables fileversionPattern.FilePath
            let fileVersionExists = fileExists filePath
            match fileVersionExists with
            | true ->
                let fileVersion = LsupEval.File.getFileVersionFromFileVersionPattern fileversionPattern
                let isMatch = (LsupEval.File.isFileVersionMatch logger fileversionPattern fileVersion)
                if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating FileVersion rule: '%A' with '%A'. Return: %b" applicabilityRule fileVersion isMatch)
                isMatch
            |false -> 
                if(logger.IsDebugEnabled) then logger.Debug(sprintf "Warning. File does not exist. Evaluating FileVersion rule: '%A'. Return: %b" applicabilityRule false)
                false
        |RegistryKeyExists registryKeyExistsPattern ->
            let registryKeyStatuses = LsupEval.Registry.getRegistryKeyStatusesFromRegistryKeyExistPattern registryKeyExistsPattern
            let isMatch = (LsupEval.Registry.isRegistryKeyMatch logger registryKeyExistsPattern registryKeyStatuses)
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating registry exists rule: '%A' with '%A'. Return: %b" applicabilityRule registryKeyStatuses isMatch)
            isMatch
        |PnPId pnPIdPattern ->                        
            let isMatch = LsupEval.PnPId.isPnPIdMatch logger pnPIdPattern systemInfo.PnPIds
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating PnPId rule: '%A'. Return: %b" applicabilityRule isMatch)
            isMatch
        |RegistryKeyValue registryKeyValuePattern ->
            let registryKeyValueStatuses = LsupEval.Registry.getRegistryKeyValueStatuses registryKeyValuePattern
            let isMatch = LsupEval.Registry.isRegistryKeyValueMatch logger registryKeyValuePattern registryKeyValueStatuses
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating registry key value rule: '%A' with '%A'. Return: %b" applicabilityRule registryKeyValueStatuses isMatch)
            isMatch
        |ExternalDetection externalDetection ->
            //1. Execute command line
            let processExitData = LsupEval.ExternalDetection.executeCommandLine externalDetection.CommandLine workingDirectory
            logger.Info(sprintf "External detection executed and exited with:\n'%A'" processExitData)
            //2. Check return codes
            let isMatch = LsupEval.ExternalDetection.returnCodeIsMatch processExitData.ExitCode externalDetection.ReturnCodes
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating external detection rule: '%A' with '%A', Exit code %A. Return: %b" applicabilityRule externalDetection processExitData.ExitCode isMatch)
            isMatch
        |Coreq coreqElement ->
            let isMatch = 
                match lsuPackages with
                |Some ps ->
                    let matchingPackages = 
                        ps 
                        |> Array.filter (fun p -> p.Name = coreqElement.Name) //Find coreq package
                        |> Array.filter (fun p ->                             //Return true if it is installed, otherwise false
                                match p.DetectInstall with
                                |Some di ->                                
                                    let detectInstallApplicabilityRule = LsupEval.Lsup.lsupXmlToApplicabilityRules logger di
                                    let isInstalled =  evaluateApplicabilityRule logger systemInfo workingDirectory lsuPackages detectInstallApplicabilityRule
                                    isInstalled
                                |None ->
                                    logger.Error(sprintf "Not possible to detect install of Coreq '%A' with '%A' because package %s do not have a detect install element." applicabilityRule coreqElement p.Name)
                                    false
                            )
                    if(Array.length matchingPackages > 0) then
                        if(logger.IsDebugEnabled) then logger.Debug(sprintf "Found that coreq package '%A' is installed." coreqElement)
                        true
                    else
                        if(logger.IsDebugEnabled) then logger.Debug(sprintf "Did not find that coreq package '%A' is installed." coreqElement)
                        false
                |None ->
                    logger.Error(sprintf "Coreq rule evaluation failed because list of update packages was not provided to the evaulation. '%A' with '%A'" applicabilityRule coreqElement)
                    false            
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "Evaluating coreq rule: '%A'. with %A. Return: %b" applicabilityRule coreqElement isMatch)
            isMatch