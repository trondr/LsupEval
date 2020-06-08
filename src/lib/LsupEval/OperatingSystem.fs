namespace LsupEval

module OperatingSystem =
    open System.Management.Automation
    open LsupEval.WmiHelper
    open LsupEval.Logging
    open LsupEval.Language

    type Os = {OsArray:string[]}

    type OsLang = 
        {
            Languages:LanguagePattern[]
        }

    let getCurrentProductType () =
        result{
            let! valueObject = getWmiPropertyValue "Win32_OperatingSystem" "OperatingSystemSKU"
            let! valueInteger = objectToUInt32 valueObject
            return  valueInteger
        }
    
    //Reference: https://techontip.wordpress.com/tag/operatingsystemsku/
    let getCurrentOperatingSystem () =
        result{
            let osVersion = System.Environment.OSVersion
            let osVersionMajor = osVersion.Version.Major
            let osVersionMinor = osVersion.Version.Minor
            let! productType = getCurrentProductType()
            return!
                match (osVersionMajor,osVersionMinor) with
                |(10,0) -> 
                    match productType with
                    |4u|27u|70u -> Result.Ok "WIN10-ENT"
                    |48u|49u|69u|103u-> Result.Ok "WIN10-PRO"
                    |_ -> Result.Ok "WIN10"
                |_ -> Result.Error (toException ("Only Windows 10 is supported.") None)
        }

    let isOperatingSystemMatch (logger:Common.Logging.ILog) (os:Os) operatingSystem =
        let v = 
            os.OsArray
            |> Seq.filter (fun s -> 
                    let isMatch = (toRegEx s).IsMatch(operatingSystem)
                    logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing operating system: '%s' with os level '%s'. Return: %b" operatingSystem s isMatch))|>ignore))
                    isMatch
                ) |>Seq.tryHead
        match v with
        |Some -> true
        |None -> false

    let toOsLang (psObject:PSObject) =
        try
            let osLanguage = psObject.Properties.["OsLanguage"].Value :?> string            
            let osLang = osLanguage.Substring(0,2).ToUpper()
            osLang            
        with
        |ex -> 
            logger.Debug(sprintf "None: '%A' %s" psObject (getAccumulatedExceptionMessages ex))
            reraise()

    let getCurrentOsLanguage() =
        try
            let osLang =
                runPowerShell( fun powershell-> 
                    powershell
                        .AddCommand("Get-ComputerInfo")
                        .AddCommand("Select-Object")
                        .AddParameter("Property",[|"OsLanguage"|])
                        .Invoke()
                    )
                    |>Seq.head 
                    |> toOsLang
            Result.Ok osLang
        with
        |ex -> Result.Error (toException "Failed to get os language." (Some ex))

    let isLanguageMatch (languagePattern:LanguagePattern) languageCode =
        let regEx = toRegEx languagePattern.LanguageCode
        let isMatch = regEx.IsMatch(languageCode)
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing language code: '%s' with language pattern '%s'. Return: %b" languageCode languagePattern.LanguageCode isMatch))|>ignore))
        isMatch

    let isOsLanguageMatch logger (osLangPatterns:LanguagePattern[]) osLang =
        match(result{
            let! language = language osLang
            let isMatch = osLangPatterns|>Array.filter(fun lp -> isLanguageMatch lp language.LanguageCode)|>Array.tryHead |> toBoolean
            return isMatch
        })with
        |Result.Ok v -> v
        |Result.Error ex -> raise ex
        

