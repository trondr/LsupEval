namespace LsupEval

module Driver = 
    open System
    open System.Diagnostics
    open System.Xml.Linq    
    open LsupEval.Logging
    let logger = getLoggerByName "LsupEval.Driver"
    
    type VersionOrLevelElement =
        |VersionElement of string
        |LevelElement of string
    
    type ServiceNameElement =
        {
            ServiceName:string
            Date:DateTime
            Version:VersionOrLevelElement
        }

    type HardwareIdElements =
        {
            HardwareIds:string[]
            Date:DateTime option
            Version:VersionOrLevelElement        
        }

    type FileElement =
        {
            FilePath:string
            Date:DateTime option
            Version:VersionOrLevelElement        
        }

    type DriverElement = 
        |HardwareIdElements of HardwareIdElements
        |FileElement of FileElement
        |ServiceNameElement of ServiceNameElement
    
    type Version = Version of string

    type HardwareInfo =
        {
            HardwareIds:string[] option
            CompatibleIds:string[]
            Name:string
            Date:DateTime
            Version:Version
            ProviderName:string
        }

    type FileInfo =
        {
            FilePath:string
            Date:DateTime
            Version:string        
        }

    type DriverInfo =
        |Hardware of HardwareInfo
        |File of FileInfo

    //driverdate on format: 20090421000000.******+***    
    let driverDateToDate (driverDate: string option) =
        match driverDate with
        |None -> None
        |Some dd -> 
            match dd with        
            |Regex @"^(\d{4})-(\d{2})-(\d{2})$" [year;month;day] -> Some (new DateTime(Convert.ToInt32(year),Convert.ToInt32(month),Convert.ToInt32(day)))
            |_ -> raise (new ArgumentException(sprintf "Device driver date format '%s' not supported" dd))

    type DataType=
        |DriverVersion
        |DriverDate

    type PnpDevicePropertyData =
        {
            InstanceId:string
            DeviceId:string
            DataType:DataType
            KeyName:string
            Data:obj
        }

    let toDataType dataType =
        match dataType with
        |"FileTime" -> DataType.DriverDate
        |"String" -> DataType.DriverVersion
        |_ -> raise (new Exception(sprintf "Data type '%s' not supported" dataType))

    let toDevicePropertyData (psObject:Management.Automation.PSObject) =
        try
            let instanceId = psObject.Properties.["InstanceId"].Value :?> string
            let deviceId = psObject.Properties.["DeviceID"].Value :?> string
            let dataType = psObject.Properties.["Type"].Value.ToString()
            let keyName = psObject.Properties.["KeyName"].Value :?> string
            let data = psObject.Properties.["Data"].Value
            Some {
                InstanceId = instanceId
                DeviceId = deviceId
                DataType = toDataType dataType
                KeyName = keyName
                Data = data
            }
        with
        |ex -> 
            logger.Debug(sprintf "None: '%A' %s" psObject (getAccumulatedExceptionMessages ex))
            None

    let getPnpDeviceProperties () =
        try
            let deviceProperties =
                runPowerShell(fun powershell ->
                    powershell
                        .AddCommand("Get-PnpDevice")                        
                        .AddCommand("Get-PnpDeviceProperty")
                        .AddParameter("KeyName",[|"DEVPKEY_Device_DriverDate";"DEVPKEY_Device_DriverVersion"|])
                        .AddCommand("Select-Object")
                        .AddParameter("Property",[|"InstanceId";"DeviceId";"Type";"KeyName";"Data"|])
                        .Invoke()
                    )|>Seq.toArray
            let devicePropertyData =
                    deviceProperties
                    |>Array.map toDevicePropertyData
                    |>Array.choose id
            Result.Ok devicePropertyData
        with
        |ex -> Result.Error (new Exception(sprintf "Failed to get device properties. %s" ex.Message, ex))
        
    type PnpDevice =
        {
            InstanceId:string
            HardwareIds:string[] option
            CompatibleIds:string[]
            Name:string
        }

    let toPnpDevice (psObject:Management.Automation.PSObject) =
        try
            let instanceId = psObject.Properties.["InstanceId"].Value :?> string
            let hardwareIds = psObject.Properties.["HardwareId"].Value :?> string[]
            let compatibleIds = psObject.Properties.["CompatibleId"].Value :?> string[]
            let name = psObject.Properties.["Name"].Value :?> string
            Some {
                InstanceId = instanceId
                HardwareIds = 
                    match hardwareIds with
                    |null -> None
                    |_ -> Some hardwareIds
                CompatibleIds=compatibleIds
                Name = name                
            }
        with
        |ex -> 
            logger.Debug(sprintf "None: '%A' %s" psObject (getAccumulatedExceptionMessages ex))
            None

    let getPnpDevices() =
        try
            let psDevices =
                runPowerShell( fun powershell-> 
                    powershell
                        .AddCommand("Get-PnpDevice")
                        .AddCommand("Select-Object")
                        .AddParameter("Property",[|"InstanceId";"HardwareId";"CompatibleId";"Name"|])
                        .Invoke()
                    )|>Seq.toArray
            let pnpDevices =
                psDevices
                |> Array.map toPnpDevice
                |> Array.choose id
            Result.Ok pnpDevices
        with
        |ex -> Result.Error (new Exception(sprintf "Failed to get devices. %s" ex.Message, ex))
    
    
    let getDriverDate (pnpDeviceProperties:PnpDevicePropertyData[]) instanceId =
        pnpDeviceProperties
        |>Array.filter(fun p -> p.InstanceId = instanceId)
        |>Array.filter(fun p -> p.DataType = DataType.DriverDate)
        |>Array.map(fun p -> p.Data :?> DateTime)
        |>Array.head

    let getDriverVersion (pnpDeviceProperties:PnpDevicePropertyData[]) instanceId =
        pnpDeviceProperties
        |>Array.filter(fun p -> p.InstanceId = instanceId)
        |>Array.filter(fun p -> p.DataType = DataType.DriverVersion)
        |>Array.map(fun p -> p.Data :?> string)
        |>Array.head
    
    let getPnpDrivers() =
            result{
                let! pnpDeviceProperties = getPnpDeviceProperties()
                let! pnpDevices = getPnpDevices()
                let pnpDevicesWithProperties =
                    query{
                        for d in pnpDevices do
                        join dp in pnpDeviceProperties on
                            (d.InstanceId = dp.InstanceId)
                        select(
                                DriverInfo.Hardware
                                    {
                                        Name=d.Name
                                        HardwareIds=d.HardwareIds
                                        CompatibleIds=d.CompatibleIds
                                        Date= getDriverDate pnpDeviceProperties d.InstanceId
                                        Version = Version (getDriverVersion pnpDeviceProperties d.InstanceId)
                                        ProviderName=""
                                    }                        
                            )                    
                    }
                return pnpDevicesWithProperties |> Seq.toArray
            }

    let getDriverFiles () =
        let driversFolder = @"c:\Windows\System32\Drivers"
        try
            Result.Ok (System.IO.Directory.GetFiles(driversFolder,"*.*"))
        with
        |ex -> Result.Error (toException (sprintf "Failed to get files from drivers folder '%s'" driversFolder) (Some ex))
    
    let getFileModifiedTime filePath =
        let fileInfo = new System.IO.FileInfo(filePath)
        fileInfo.LastWriteTime

    let getFileDrivers files =
        files
        |>Seq.map(fun f -> 
                DriverInfo.File {
                   FilePath = f
                   Date = getFileModifiedTime f
                   Version = LsupEval.File.getFileVersion f
                })

    let getCurrentDrivers () =
        result{
            //Get pnp info
            let! pnpDrivers = getPnpDrivers()
            //Get file info
            let! fileNames = getDriverFiles()
            let fileDrivers = getFileDrivers(fileNames) |> Seq.toArray
            let drivers = Array.append (pnpDrivers|> Seq.toArray) fileDrivers
            return drivers
        }

    let getFileDriversFromDriverPattern driver = 
        match driver with
        |DriverElement.FileElement f -> 
            let filePath = LsupEval.File.resolveFilePath f.FilePath
            if(System.IO.File.Exists(filePath)) then
                [|DriverInfo.File {
                    FilePath = filePath
                    Date = getFileModifiedTime filePath
                    Version = LsupEval.File.getFileVersion filePath
                }|]
            else
                [||]
        |DriverElement.HardwareIdElements _ -> [||]
        |DriverElement.ServiceNameElement _ -> [||]

    let toDriverDate (xElement:XElement) =        
        let driverDateElement = toOption (xElement.Element(xn "Date"))                        
        let date =
            match driverDateElement with
            |None -> None
            |Some d -> 
                let dateString = d.Value
                driverDateToDate (Some dateString)
        date

    let toDriverVersion (xElement:XElement) =
        let versionElement = toOption (xElement.Element(xn "Version"))
        let version = 
            match versionElement with
            |None -> raise (new NotSupportedException("toDriverVersion failed."))
            |Some v -> v.Value
        version

    let getOptionalFileElement (driverXElement:XElement) =        
        match(driverXElement.Element(xn "File") |> toOption) with         
        |None -> None
        |Some x -> 
                let version = toDriverVersion driverXElement
                let date = toDriverDate driverXElement
                Some (  FileElement         
                            {
                                FilePath=x.Value
                                Date=date
                                Version=VersionElement version        
                            }
                    )

    let getOptionalHardwareIdElements (driverXElement:XElement) =
        let hardwareIds = 
            (driverXElement.Elements(xn "HardwareID"))
            |>Seq.map(fun x -> x.Value)
            |>Seq.toArray
        let version = toDriverVersion driverXElement
        let date = toDriverDate driverXElement
        match hardwareIds with
        |[||] -> None
        | ids -> 
            Some(
                HardwareIdElements
                        {
                            HardwareIds=ids 
                            Date=date
                            Version = VersionElement version
                        }
                )

    let isHardwareIdMatch hardwareId hardwareIdPattern =
        let regex = toRegEx hardwareIdPattern
        regex.IsMatch(hardwareId)

    ///Remove time information from the date
    let absoluteDate (dateTime:DateTime) =
        new DateTime(dateTime.Year,dateTime.Month,dateTime.Day)        
            
    let isDriverDateMatch (hardwareInfoDriverDate:DateTime) (hardwareIdElementDriverDate:DateTime option) =
        let isMatch =
            match hardwareIdElementDriverDate with
            |None -> true
            |Some date ->         
                (absoluteDate hardwareInfoDriverDate) <= (absoluteDate date)
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing driver date: '%A' with driver date pattern '%A'. Return: %b" (Some hardwareInfoDriverDate) hardwareIdElementDriverDate isMatch))|>ignore))
        isMatch

    let isDriverVersionMatch currentDriverVersion newDriverVersion =
        let isMatch =
            match(result{
                let! currentVersion = LsupEval.Version.version currentDriverVersion
                let! versionPattern = LsupEval.Version.versionPattern newDriverVersion
                let isMatch = LsupEval.Version.isVersionPatternMatch currentVersion versionPattern
                return isMatch
            }) with
            |Result.Ok isMatch -> isMatch
            |Result.Error ex -> 
                logger.Error(sprintf "Failed to match version '%s' with pattern '%s'." currentDriverVersion newDriverVersion)
                false
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing driver version: '%A' with driver version pattern '%A'. Return: %b" currentDriverVersion newDriverVersion isMatch))|>ignore))
        isMatch


    let isHardwareMatch (driverElement:HardwareIdElements) (hardwareInfo:HardwareInfo) =        
        let hardwareIdIsMatch = 
            match hardwareInfo.HardwareIds with
            |Some hwids ->            
                hwids
                |> Seq.filter(fun s -> (any (isHardwareIdMatch s) driverElement.HardwareIds))|>Seq.tryHead|>toBoolean
            |None -> false
        let dateIsMatch = isDriverDateMatch hardwareInfo.Date driverElement.Date
        
        let cv (Version version) =
            version

        let nv (version:VersionOrLevelElement) =
            match version with
            |VersionElement v -> v
            |LevelElement l -> l            
        let versionIsMatch = isDriverVersionMatch (cv hardwareInfo.Version) (nv driverElement.Version)
        let isMatch = hardwareIdIsMatch && dateIsMatch && versionIsMatch
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing hardware: '%A' with hardware pattern '%A'. (HardwareIdIsMatch=%b && DateIsMatch=%b && VersionIsMatch=%b) Return: %b" hardwareInfo driverElement hardwareIdIsMatch dateIsMatch versionIsMatch isMatch))|>ignore))
        isMatch

    let isDriverFileMatch (patternFile:FileElement) (infoFile:FileInfo) =
        let filePath = LsupEval.File.resolveFilePath patternFile.FilePath
        let isFilePathMatch = LsupEval.File.isFilePathMatch filePath infoFile.FilePath
        let isVersionMatch = 
            match patternFile.Version with
            |VersionOrLevelElement.VersionElement v ->
                isDriverVersionMatch infoFile.Version v
            |VersionOrLevelElement.LevelElement l -> 
                raise (new NotImplementedException("isDriverFileMatch: Evaluation of VersionOrLevelElement.LevelElement"))
        let isDriverDateMatch = isDriverDateMatch infoFile.Date patternFile.Date
        let isMatch = isFilePathMatch && isVersionMatch && isDriverDateMatch
        isMatch

    let isDriverMatch (logger:Common.Logging.ILog) driver (installedDrivers:seq<DriverInfo>) =
        let isDriverMatch =
            match driver with
            |HardwareIdElements hw ->                        
                let isMatch =
                    installedDrivers
                    |>Seq.map(fun d -> 
                        match d with
                        |DriverInfo.Hardware h -> Some h
                        |DriverInfo.File _ -> None
                        )
                    |>Seq.choose id
                    |>Seq.filter (fun h -> isHardwareMatch hw h)
                    |>Seq.tryHead |> toBoolean
                logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing hardware id version: '%A' with driver pattern '%A'. Return: %b" installedDrivers hw isMatch))|>ignore))
                isMatch            
            |FileElement fileElement -> 
                let isMatch =
                    installedDrivers
                    |>Seq.map(fun d -> 
                        match d with
                        |DriverInfo.Hardware _ -> None
                        |DriverInfo.File f -> Some f
                        )
                    |>Seq.choose id
                    |>Seq.filter (fun file -> (isDriverFileMatch fileElement file))
                    |>Seq.tryHead |> toBoolean
                logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing driver file version: '%A' with driver file pattern '%A'. Return: %b" installedDrivers fileElement isMatch))|>ignore))
                isMatch            
            |ServiceNameElement s ->
                raise (new NotImplementedException("Evaluation of ServiceNameElement"))
        isDriverMatch
