namespace LsupEval

module Lsup =
    open System
    open System.Xml.Linq

    type LsuRebootType = Default=0|RebootForced=1|Reserved=2|RebootRequired=3|Shutdown=4|RebootDelayed=5
    type LsuSeverityType = Default=0|Critical=1|Recommended=2|Optional=3|Extra=9|Offer=10
    type LsuBrandType = All=1|Think=2|LenovoNotebook=3|LenovoDesktop=4
    type LsuPackageType = Others=0|Application=1|Drivers=2|Bios=3|Firmware=4
            
    type LsuNlsFile =
        {
            Id:string option
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
            Files:LsuNlsFile[]
        }
    
    type License =
        {
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

    let loadLsuPackageXDocument (filePath:string) =
        try
            Result.Ok (XDocument.Load(filePath))
        with
        |ex ->
            Result.Error (toException (sprintf "Failed to load '%s'. %s" filePath  ex.Message) (Some ex))

    let loadLsuPackageXElement (lsuPackageXDocument:XDocument) =        
            Result.Ok lsuPackageXDocument.Root
     
    let xn s = 
        XName.Get(s)

    let getRequiredAttribute (xElement:XElement) (attributeName:string) =
        match xElement with
        | null -> Result.Error (new Exception(sprintf "Element is null. Failed to get attribute name '%s'" attributeName))
        |_ -> 
            let attribute = xElement.Attribute(xn attributeName)
            match attribute with
            |null -> Result.Error (new Exception(sprintf "Attribute '%s' not found on element: %A" attributeName xElement))
            |_ -> Result.Ok attribute.Value

    let toResult value errorMessage =
        match value with
        | null -> Result.Error (new Exception(errorMessage))
        | _ -> Result.Ok value

    let getChildXElement (parentXElement:XElement) (elementName:string) =
        toResult (parentXElement.Element(xn elementName)) (sprintf "Element '%s' not found on parent element: '%A'" elementName parentXElement)



    let getOptionalChildXElement (parentXElement:XElement) (elementName:string) =
        match((getChildXElement parentXElement elementName)) with
        |Ok v -> Some v
        |Error _ -> None

    let toBoolean str =
        match str with
        |"False" -> false
        |"True" -> true
        |_ -> false

    let toRebootType (rebootType:int) : LsuRebootType =
        LanguagePrimitives.EnumOfValue rebootType

    let innerXml (xElement:XElement) = 
        xElement.Nodes()
        |>Seq.filter(fun n -> (n.NodeType = System.Xml.XmlNodeType.Element)||(n.NodeType = System.Xml.XmlNodeType.Text))
        |>Seq.map(fun n -> n.ToString())
        |>Seq.toArray
        |>String.concat ""

    let toOptionalInnerXml (xElement:XElement option) =
        match xElement with
        |Some v -> Some (innerXml v)
        |None -> None

    let toInteger64 (value:string) =
        try
            Result.Ok (Convert.ToInt64(value))
        with
        |ex -> Result.Error (toException (sprintf "Failed to convert %s to integer value." value) (Some ex))

    let getLsuFile fileXElement =
        result{
            let! nameElement = getChildXElement fileXElement "Name"
            let name = nameElement.Value
            let! crcElement = getChildXElement fileXElement "CRC"
            let crc = crcElement.Value

            let! sizeXElement = getChildXElement fileXElement "Size"
            let! size = toInteger64 sizeXElement.Value
            return
                {
                    LsuFile.Name = name
                    Crc = crc
                    Size= size            
                }
        }

    let getLsuFiles lsuPackageXElement =
        result{
            let! filesXElement = getChildXElement lsuPackageXElement "Files"
            let! installerXElement = getChildXElement filesXElement "Installer"
            let installerFilesXElements = installerXElement.Elements(xn "File")
            let! installerFiles = installerFilesXElements|>Seq.map(fun fileXElement -> getLsuFile fileXElement) |> toAccumulatedResult

            let externalXElement = getOptionalChildXElement filesXElement "External"                        
            let external =
                match externalXElement with
                |None-> None
                |Some extXElement ->                                             
                        let externalFilesXElements = extXElement.Elements(xn "File")
                        let externalFiles = externalFilesXElements|>Seq.map(fun fileXElement -> getLsuFile fileXElement) |> getAllValues |>Seq.toArray
                        Some {External.Files= externalFiles}
                    
            return
                {
                    Installer = {Installer.Files = installerFiles|>Seq.toArray}
                    Readme = None
                    License = None
                    External = external
                    AppIcon = None
                }
        }

    let loadLsuPackage (lsuPackageXElement:XElement) =
        result{
            let! id = getRequiredAttribute lsuPackageXElement "id"
            let! name = getRequiredAttribute lsuPackageXElement "name"
            let! version = getRequiredAttribute lsuPackageXElement "version"
            let! hide = getRequiredAttribute lsuPackageXElement "hide"
            let! rebootXElement = getChildXElement lsuPackageXElement "Reboot"
            let! rebootType = getRequiredAttribute rebootXElement "type"

            let! titleXElement = getChildXElement lsuPackageXElement "Title"
            let! defaultLanguageCode = getRequiredAttribute titleXElement "default"
            let descXElment = titleXElement.Elements(xn "Desc") |> Seq.tryFind (fun e -> e.Attribute(xn "id").Value = defaultLanguageCode)
            let title = 
                match descXElment with
                |None -> "<Unknown title>"
                |Some t -> t.Value
            
            let detectInstallXElement = getOptionalChildXElement lsuPackageXElement "DetectInstall"
            let detectInstall = toOptionalInnerXml detectInstallXElement
            
            let dependenciesXElement = getOptionalChildXElement lsuPackageXElement "Dependencies"
            let dependencies = toOptionalInnerXml dependenciesXElement

            let! files = getLsuFiles lsuPackageXElement

            return {
                    Id = id
                    Name = name
                    Version=version
                    Hide=toBoolean hide
                    Title = title
                    ReleaseDate=DateTime.Now
                    RebootType=toRebootType (Convert.ToInt32(rebootType))
                    DetectInstall = detectInstall
                    Dependencies=dependencies
                    Files=files
                }                
        }
