namespace LsupEval

module LsupData=
    open System
    open LsupEval.Bios
    open LsupEval.Cpu
    open LsupEval.OperatingSystem
    open LsupEval.Driver
    open LsupEval.EmbeddedController
    open LsupEval.File
    open LsupEval.Registry
    open LsupEval.PnPId
    open LsupEval.ExternalDetection
    open LsupEval.Coreq    
    open MBrace.FsPickler

    type LsuRebootType = Default=0|RebootForced=1|Reserved=2|RebootRequired=3|Shutdown=4|RebootDelayed=5
    type LsuSeverityType = Default=0|Critical=1|Recommended=2|Optional=3|Extra=9|Offer=10
    type LsuBrandType = All=1|Think=2|LenovoNotebook=3|LenovoDesktop=4
    type LsuPackageType = Others=0|Application=1|Drivers=2|Bios=3|Firmware=4
            
    type LsuNlsFile =
        {
            Id:string
            Name:string
            Crc:string
            Size:Int64
        }

    type LsuFile =
        {            
            Name:string
            Crc:string
            Size:Int64
        }

    type Installer =
        {
            Files:LsuFile[]
        }

    type Readme =
        {
            Default:string
            Files:LsuNlsFile[]
        }
    
    type License =
        {
            Default:string
            Files:LsuNlsFile[]
        }

    type External =
        {
            Files:LsuFile[]
        }

    type AppIcon =
        {
            File:LsuFile
        }

    type LsuFiles =
        {
            Installer:Installer
            Readme:Readme option
            License:License option
            External:External option
            AppIcon:AppIcon option
        }

    type LsuPackage = {
        Id:string
        Name:string
        Version:string
        Hide:bool
        Title:string
        ReleaseDate:DateTime
        RebootType:LsuRebootType
        DetectInstall:string option
        Dependencies:string option
        Files:LsuFiles
    }



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
        |Coreq of CoreqElement
        
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