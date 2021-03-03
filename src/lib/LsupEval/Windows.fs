namespace LsupEval

module Windows =
    open System
    open LsupEval.Registry
    open LsupEval.Version

    type WindowsBuildVersion =
        |VersionElement of string

    let isWindowsBuildVersionMatch (logger:Common.Logging.ILog) (windowsBuildVersionLevel:WindowsBuildVersion) (currentWindowsBuildVersion:string) =
        let versionLevel = 
            match(windowsBuildVersionLevel) with
            |VersionElement ve -> ve
        let lsupVersionPattern = versionPatternUnsafe versionLevel
        let lsupVersion = versionUnsafe currentWindowsBuildVersion

        isVersionPatternMatch lsupVersion lsupVersionPattern
        
    let getWindowsBuildVersion () =        
        let registryKey =  
            {
                Hive = Microsoft.Win32.Registry.LocalMachine
                SubKeyPath = "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
            }
        let registryValue = getRegistryValue registryKey "CurrentBuildNumber"
        match registryValue.Value with
        |None -> raise (new Exception("Windows build version not found in registry ([HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion]CurrentBuildNumber)."))
        |Some cbn -> cbn.Value :?> string