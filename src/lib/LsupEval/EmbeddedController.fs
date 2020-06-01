namespace LsupEval

module EmbeddedController =
    open LsupEval.Logging
    open System.Management.Automation
    let logger = getLoggerByName "LsupEval.EmbeddedController"

    let toEmbeddedControllerVersion (psObject:PSObject) =
        try
            let majorVersion = psObject.Properties.["BiosEmbeddedControllerMajorVersion"].Value :?> int16
            let minorVersion = psObject.Properties.["BiosEmbeddedControllerMinorVersion"].Value :?> int16
            sprintf "%i.%i" majorVersion minorVersion
        with
        |ex -> 
            logger.Debug(sprintf "None: '%A' %s" psObject (getAccumulatedExceptionMessages ex))
            reraise()

    let getCurrentEmbeddedControllerVersion () =        
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
        embeddedControllerVersion


