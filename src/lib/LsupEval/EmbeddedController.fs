namespace LsupEval

module EmbeddedController =
    open LsupEval.Logging
    open LsupEval.Version
    open System.Management.Automation
    let logger = getLoggerByName "LsupEval.EmbeddedController"

    type EmbeddedControllerVersionElement = {Versions:string[]}

    let toEmbeddedControllerVersion (psObject:PSObject) =
        try
            let majorVersion = psObject.Properties.["BiosEmbeddedControllerMajorVersion"].Value :?> int16
            let minorVersion = psObject.Properties.["BiosEmbeddedControllerMinorVersion"].Value :?> int16
            sprintf "%i.%i" majorVersion minorVersion
        with
        |ex -> 
            if(logger.IsDebugEnabled) then logger.Debug(sprintf "None: '%A' %s" psObject (getAccumulatedExceptionMessages ex))
            reraise()

    let getCurrentEmbeddedControllerVersion () =        
        try
            let embeddedControllerVersion =
                runPowerShell( fun powershell-> 
                    powershell
                        .AddCommand("Get-ComputerInfo")
                        .AddCommand("Select-Object")
                        .AddParameter("Property",[|"BiosEmbeddedControllerMajorVersion";"BiosEmbeddedControllerMinorVersion"|])
                        .Invoke()
                    )
                    |>Seq.head 
                    |> toEmbeddedControllerVersion
            Result.Ok embeddedControllerVersion
        with
        |ex -> Result.Error (toException "Failed to get embedded controller version." (Some ex))

    let isEmbeddedControllerVersionMatch (logger:Common.Logging.ILog) (embeddedControllerVersion:EmbeddedControllerVersionElement) systemInfoEmbeddedControllerVersion =        
        match(result{
            let! lsupVersionPatterns =
                embeddedControllerVersion.Versions
                |>Seq.map(fun v -> LsupEval.Version.versionPattern v)
                |>toAccumulatedResult    
            let! lsupVersion = (version systemInfoEmbeddedControllerVersion)
            let isMatch =
                lsupVersionPatterns
                |>Seq.filter(fun vp -> 
                        let isMatch = isVersionPatternMatch lsupVersion vp
                        if(logger.IsDebugEnabled) then logger.Debug(sprintf "Comparing embedded controller version: '%A' with embedded controller pattern '%A'. Return: %b" systemInfoEmbeddedControllerVersion vp isMatch)
                        isMatch
                    )
                |>Seq.tryHead|>toBoolean
            return isMatch
        })with
        |Result.Ok isMatch -> isMatch
        |Result.Error ex -> raise ex
