namespace LsupEval

module Driver = 
    open System
    open System.Diagnostics
    open LsupEval.WmiHelper
    
    type VersionPattern =
        |Version of string
        |Level of string
    
    type DriverService =
        {
            ServiceName:string
            Date:DateTime
            Version:VersionPattern
        }

    type DriverHardwareId =
        {
            HardwareId:string
            Date:DateTime option
            Version:VersionPattern        
        }

    type DriverFile =
        {
            FilePath:string
            Date:DateTime option
            Version:VersionPattern        
        }

    type Driver = 
        |HardwareId of DriverHardwareId
        |File of DriverFile
        |Service of DriverService
    
    type Version = Version of string

    type HardwareInfo =
        {
            Id:string
            Name:string
            Date:DateTime option
            Version:Version
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
    let toDriverDatetoDate (driverDate: string option) =
        match driverDate with
        |None -> None
        |Some dd -> 
            match dd with        
            |Regex @"^(\d{4})(\d{2})(\d{2})$" [year;month;day] -> Some (new DateTime(Convert.ToInt32(year),Convert.ToInt32(month),Convert.ToInt32(day)))
            |_ -> raise (new ArgumentException(sprintf "Device driver date format '%s' not supported" dd))

    //Get-WmiObject Win32_PnPSignedDriver| select devicename, driverversion, deviceid,driverdate
    let getPnpDrivers() =
        result{
            let propertyNames = [|"DeviceName";"HardwareId";"DriverVersion";"DriverDate"|]
            let! wmiObjects = WmiHelper.getWmiObjects "Win32_PnPSignedDriver" propertyNames            
            let! drivers =
                wmiObjects
                |>Seq.map(fun m ->   
                    result{
                        let! id=objectToString m.["HardwareId"]
                        let! name = objectToString m.["DeviceName"]
                        let! dateString = (objectToString m.["DriverDate"])
                        let date= toDriverDatetoDate (Some dateString)
                        let! version= objectToString m.["DriverVersion"]
                        return 
                            DriverInfo.Hardware 
                            {
                                Id=id
                                Name=name
                                Date=date
                                Version=Version version
                            }
                    }
                )|>toAccumulatedResult
            return drivers
        }

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
        |Driver.File f -> 
            [|DriverInfo.File {
                FilePath = f.FilePath
                Date = getFileModifiedTime f.FilePath
                Version = getFileVersion f.FilePath
            }|]
        |Driver.HardwareId _ -> [||]
        |Driver.Service _ -> [||]

    let isDriverMatch logger driver drivers =
        raise (new NotImplementedException())
        false