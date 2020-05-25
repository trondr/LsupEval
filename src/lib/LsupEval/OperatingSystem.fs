namespace LsupEval

module OperatingSystem =
    open LsupEval.WmiHelper
    open LsupEval.Logging

    type Os = {OsArray:string[]}

    let getCurrentProductType () =
        result{
            let! valueObject = getWmiPropertyValue "Win32_OperatingSystem" "OperatingSystemSKU"
            let! valueInteger = objectToInt32 valueObject
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
                    |4|27|70 -> Result.Ok "WIN10-ENT"
                    |48|49|69|103-> Result.Ok "WIN10-PRO"
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