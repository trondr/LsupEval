namespace LsupEval

module Driver = 
    open System
    open System.Diagnostics
    open System.Xml.Linq
    open LsupEval.WmiHelper
    
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
            HardwareIds:string[]
            CompatibleIds:string[]
            Name:string
            Date:DateTime option
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

    //Get-WmiObject Win32_PnPSignedDriver| select devicename, driverversion, deviceid,driverdate
    let getPnpDrivers() =
        result{
            let propertyNames = [|"DeviceName";"HardwareId";"DriverVersion";"DriverDate";"DriverProviderName"|]
            let! wmiObjects = WmiHelper.getWmiObjects "Win32_PnPSignedDriver" propertyNames            
            let! drivers =
                wmiObjects
                |>Seq.map(fun m ->   
                    result{
                        let! id=objectToString m.["HardwareId"]
                        let! name = objectToString m.["DeviceName"]
                        let! dateString = (objectToString m.["DriverDate"])
                        let date= driverDateToDate (Some dateString)
                        let! version= objectToString m.["DriverVersion"]
                        let! driverProviderName = objectToString m.["DriverProviderName"]
                        return 
                            DriverInfo.Hardware 
                            {
                                HardwareIds=[|id|]
                                CompatibleIds=[||]
                                Name=name
                                Date=date
                                Version=Version version
                                ProviderName=driverProviderName
                            }
                    }
                )|>toAccumulatedResult
            return drivers
        }

    type PnPDevices =
        {
            InstanceId:string
            HardwareIds:string[]
            CompatibleIds:string[]
            Name:string            
            Version:string
            ProviderName:string
        }
    
    type PnpInstanceIdAndDriverDate=
        {
            InstanceId:string
            DriverDate:DateTime
        }

    let getPnpDeviceDriverDate instanceId =
        runPowerShell(fun powershell-> 
            powershell                                                                        
                .AddCommand("Get-PnpDeviceProperty")
                .AddParameter("InstanceId",instanceId)
                .AddParameter("KeyName","DEVPKEY_Device_DriverDate")
                .AddCommand("Select-Object")
                .AddParameter("Property",[|"InstanceId";"Data"|])
                .Invoke()
            )
            |>Seq.map(fun dd ->     
                let instanceId = dd.Properties.["InstanceId"].Value :?> string
                let dateTime = dd.Properties.["Data"].Value :?> DateTime
                {
                    InstanceId = instanceId
                    DriverDate = dateTime
                }
                )|>Seq.head

    let getPnpDevices() =
        try
            let pnpDevices = 
                runPowerShell( fun powershell-> 
                    powershell
                        .AddCommand("Get-PnpDevice")
                        .AddCommand("Select-Object")
                        .AddParameter("Property",[|"InstanceId";"HardwareId";"Name"|])                        
                        .Invoke()
                    )|>Seq.map(fun d->
                        let instanceId = (d.Properties.["InstanceId"].Value :?> string)
                        let hardwareIds = (d.Properties.["HardwareId"].Value :?> string[])
                        {
                            InstanceId= instanceId
                            HardwareIds=hardwareIds
                            CompatibleIds=  [||]//(d.Properties.["CompatibleID"].Value :?> string[])
                            Name=(d.Properties.["Name"].Value :?> string)
                            Version="1.0.0.0"
                            ProviderName="Microsoft"
                        }
                    )|>Seq.toArray

            let instanceIds = pnpDevices|>Seq.map(fun d -> d.InstanceId)|>Seq.toArray|>Array.head

            let pnpDeviceDriverDates =
                    runPowerShell(fun powershell-> 
                                        powershell                                                                        
                                            .AddCommand("Get-PnpDeviceProperty")
                                            .AddParameter("InstanceId",instanceIds)
                                            .AddParameter("KeyName","DEVPKEY_Device_DriverDate")
                                            .AddCommand("Select-Object")
                                            .AddParameter("Property",[|"InstanceId";"Data"|])
                                            .Invoke()
                                  )
                                  |>Seq.map(fun dd ->     
                                                let instanceId = dd.Properties.["InstanceId"].Value :?> string
                                                let dateTime = dd.Properties.["Data"].Value :?> DateTime
                                                {
                                                    InstanceId = instanceId
                                                    DriverDate = dateTime
                                                }
                                  )|>Seq.toArray

            printfn "%A" pnpDeviceDriverDates

            let devices = 
                query{
                    for d in pnpDevices do
                    join dd in pnpDeviceDriverDates on
                        (d.InstanceId = dd.InstanceId)
                    select (
                                DriverInfo.Hardware 
                                    {
                                        HardwareIds=d.HardwareIds
                                        CompatibleIds=d.CompatibleIds
                                        Name=d.Name
                                        Date=Some dd.DriverDate
                                        Version=Version d.Version
                                        ProviderName=d.ProviderName
                                    }
                            )

                }
            Result.Ok (devices|>Seq.toArray)
        with
        |ex -> Result.Error ex

    let getDriverFiles () =
        let driversFolder = @"c:\Windows\System32\Drivers"
        try
            Result.Ok (System.IO.Directory.GetFiles(driversFolder,"*.*"))
        with
        |ex -> Result.Error (toException (sprintf "Failed to get files from drivers folder '%s'" driversFolder) (Some ex))
    
    let getFileVersion filePath =
        let fileVersion = FileVersionInfo.GetVersionInfo(filePath);
        (sprintf "%d.%d.%d.%d" fileVersion.FileMajorPart fileVersion.FileMinorPart fileVersion.FileBuildPart fileVersion.FilePrivatePart)
        
    let getFileModifiedTime filePath =
        let fileInfo = new System.IO.FileInfo(filePath)
        fileInfo.LastWriteTime

    let getFileDrivers files =
        files
        |>Seq.map(fun f -> 
                DriverInfo.File {
                   FilePath = f
                   Date = getFileModifiedTime f
                   Version = getFileVersion f
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
            [|DriverInfo.File {
                FilePath = f.FilePath
                Date = getFileModifiedTime f.FilePath
                Version = getFileVersion f.FilePath
            }|]
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
                let version = toDriverVersion x
                let date = toDriverDate x
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

    let isDriverMatch logger driver drivers =
        match driver with
        |HardwareIdElements hw ->                        
            raise (new NotImplementedException("Evaluation of HardwareIdElements"))
        |FileElement f -> 
            raise (new NotImplementedException("Evaluation of FileElement"))
        |ServiceNameElement s ->
            raise (new NotImplementedException("Evaluation of ServiceNameElement"))
