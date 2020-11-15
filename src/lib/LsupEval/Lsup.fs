namespace LsupEval

module Lsup =
    open System
    open System.Xml
    open System.Xml.Linq    
    open LsupEval.Logging
    open LsupEval.Registry
    open LsupEval.LsupData
    let logger = Logging.getLoggerByName "Lsup"
       

    let loadLsuPackageXDocument (filePath:string) =
        try
            Result.Ok (XDocument.Load(filePath))
        with
        |ex ->
            Result.Error (toException (sprintf "Failed to load '%s'. %s" filePath  ex.Message) (Some ex))

    let loadLsuPackageXDocumentFromString (xmlString:string) =
        try
            Result.Ok (XDocument.Parse(xmlString))
        with
        |ex ->
            Result.Error (toException (sprintf "Failed to load '%s'. %s" xmlString  ex.Message) (Some ex))

    let loadLsuPackageXElement (lsuPackageXDocument:XDocument) =        
            Result.Ok lsuPackageXDocument.Root
    
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
        |Some v -> 
            let xml = innerXml v
            match(System.String.IsNullOrWhiteSpace(xml)) with
            |true -> None
            |false -> Some xml
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

    let getLsuNlsFile fileXElement =
        result{
            let! lsuFile = getLsuFile fileXElement
            let! id = getRequiredAttribute fileXElement "id"
            return
                {
                    LsuNlsFile.Id = id
                    Name = lsuFile.Name
                    Crc = lsuFile.Crc
                    Size= lsuFile.Size     
                }
        }

    let getOptionalLsuFiles filesXElement elementName =
        let xElement = getOptionalChildXElement filesXElement elementName                        
        let optionalLsuFiles =
            match xElement with
            |None-> None
            |Some extXElement ->                                             
                    let fileXElements = extXElement.Elements(xn "File")
                    let files = fileXElements|>Seq.map(fun fileXElement -> getLsuFile fileXElement) |> getAllValues |>Seq.toArray
                    Some files
        optionalLsuFiles

    let getOptionalLsuNlsFiles filesXElement elementName =
        let xElement = getOptionalChildXElement filesXElement elementName                                
        let optionalLsuFiles =
            match xElement with
            |None-> None
            |Some extXElement ->                                             
                    match(result{
                        let! id = getRequiredAttribute extXElement "default"                                        
                        let fileXElements = extXElement.Elements(xn "File")
                        let files = fileXElements|>Seq.map(fun fileXElement -> getLsuNlsFile fileXElement) |> getAllValues |>Seq.toArray
                        return Some (id,files)
                    })with
                    |Result.Ok v -> v
                    |Result.Error ex -> 
                        logger.Warn (sprintf "Failed to get %s element. %s" elementName ex.Message)
                        None

        optionalLsuFiles

    let getLsuFiles lsuPackageXElement =
        result {
            let! filesXElement = getChildXElement lsuPackageXElement "Files"
            let! installerXElement = getChildXElement filesXElement "Installer"
            let installerFilesXElements = installerXElement.Elements(xn "File")
            let! installerFiles = installerFilesXElements|>Seq.map(fun fileXElement -> getLsuFile fileXElement) |> toAccumulatedResult

            let external =
                match(getOptionalLsuFiles filesXElement "External") with
                |Some f -> Some {External.Files= f}
                |None -> None

            let readme = 
                match(getOptionalLsuNlsFiles filesXElement "Readme") with
                |Some (id,f) ->                                     
                    Some {Readme.Files= f;Default=id}
                |None -> None

            return
                {
                    Installer = {Installer.Files = installerFiles|>Seq.toArray}
                    Readme = readme
                    License = None //Not implemented
                    External = external
                    AppIcon = None //Not implemented
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
    
    let internal getAttribute (xElement:XElement) (attributeName:string) defaultValue =
        let attributeValue = xElement.Attribute(xn attributeName)
        if(attributeValue = null) then
            defaultValue()
        else
            attributeValue.Value

    let internal toBios (xElement:XElement) =
        let levels = xElement.Elements(xn "Level")
        let versions = levels|>Seq.map (fun (x:XElement) -> x.Value)|>Seq.toArray
        ApplicabilityRule.Bios {Versions=versions}

    let internal toCpuAddressWidth (xElement:XElement) =
        let addressWidth = xElement.Element(xn "AddressWidth")
        let cpuAddressWidth =  Convert.ToUInt16(addressWidth.Value)
        let cpuAddressWidth: Cpu.CpuAddressWidth = LanguagePrimitives.EnumOfValue cpuAddressWidth
        ApplicabilityRule.CpuAddressWidth cpuAddressWidth

    let internal toOperatingSystem (xElement:XElement) =
        let osElements = xElement.Elements(xn "OS")
        let osArray = osElements|>Seq.map (fun (x:XElement) -> x.Value)|>Seq.toArray
        ApplicabilityRule.Os {OsArray=osArray}

    let toDriver (driverXElement:XElement) =
        let hardwareIds = driverXElement |> Driver.getOptionalHardwareIdElements
        let file = driverXElement |> Driver.getOptionalFileElement
        let driver = [|hardwareIds;file|]|>Array.choose id|>Array.head
        ApplicabilityRule.Driver driver

    let toEmbeddedControllerVersion (embeddedControllerVersionXElement:XElement) =
        let versions = embeddedControllerVersionXElement.Elements(xn "Version")
        let versionArray = versions|>Seq.map(fun (x:XElement) -> x.Value) |> Seq.toArray
        ApplicabilityRule.EmbeddedControllerVersion {Versions=versionArray}

    let toFileExists (fileExistsXElement:XElement) =
        let filePath = fileExistsXElement.Value
        ApplicabilityRule.FileExists {FilePath=filePath}

    let toFileVersion (xElement:XElement) =
        let fileElement = xElement.Element(xn "File")
        let versionElement = xElement.Element(xn "Version")        
        ApplicabilityRule.FileVersion
            {
                FilePath=fileElement.Value
                VersionPattern=LsupEval.Version.versionPatternUnsafe versionElement.Value
            }

    let toOsLanguage (xElement:XElement) =
        let langElements = xElement.Elements(xn "Lang")
        let osLangArray = langElements|>Seq.map (fun (x:XElement) -> LsupEval.Language.languageUnsafe x.Value)|>Seq.toArray
        ApplicabilityRule.OsLang
            {
                Languages = osLangArray
            }

    let toRegistryKey (xElement:XElement) =
        let registryKeyElements = xElement.Elements(xn "Key")
        let registryKeys = registryKeyElements |> Seq.map(fun (x:XElement) -> LsupEval.Registry.registryKeyUnsafe x.Value)|>Seq.toArray
        ApplicabilityRule.RegistryKeyExists
            {
                RegistryKeys=registryKeys
            }
    
    let toPnPId (pnpIdXElement:XElement) = 
        ApplicabilityRule.PnPId
            {
                PnPId =pnpIdXElement.Value
            }

    let toRegistryKeyValue (xElement:XElement) =
        match(result{
            let! regType = getRequiredAttribute xElement "type"
            let keyXElement = xElement.Element(xn "Key")
            let valueNameXElement = xElement.Element(xn "KeyName")
            let versionXElement = getOptionalChildXElement xElement "Version"
            let valueXElement = getOptionalChildXElement xElement  "KeyValue"
            let levelXElement = getOptionalChildXElement xElement  "Level"
            return
                match versionXElement with
                |Some vx -> 
                    {
                        Key = LsupEval.Registry.toRegistryKey keyXElement.Value                            
                        ValueName = valueNameXElement.Value
                        ValueKind = (LsupEval.Registry.toRegistryValueKind regType)
                        Value = LsupEval.Registry.ValuePattern.Version vx.Value
                    }
                |None->
                    match valueXElement with
                    |Some vx -> 
                        {
                            Key = LsupEval.Registry.toRegistryKey keyXElement.Value
                            ValueName = valueNameXElement.Value
                            ValueKind = (LsupEval.Registry.toRegistryValueKind regType)
                            Value = LsupEval.Registry.ValuePattern.Value vx.Value
                        }
                    |None -> 
                        match levelXElement with
                        |Some lx ->
                            {
                                Key = LsupEval.Registry.toRegistryKey keyXElement.Value
                                ValueName = valueNameXElement.Value
                                ValueKind = (LsupEval.Registry.toRegistryValueKind regType)
                                Value = LsupEval.Registry.ValuePattern.Level lx.Value
                            }
                        |None -> failwith "Invalid RegistryKeyValue element. Missing either KeyName, Version or Level"
        })with
        |Result.Ok rv -> ApplicabilityRule.RegistryKeyValue rv
        |Result.Error ex -> raise ex
       
    let toInt32 (integerString:string) =
        try
            Some (Convert.ToInt32(integerString))
        with
        |ex -> 
            logger.Warn(sprintf "Failed to convert '%s' to integer due to: %s" integerString ex.Message)
            None

    let toIntArray (stringOfInts:string) =
        let intArray = 
            stringOfInts.Split(',')
            |>Array.map(fun s-> s.Trim())
            |>Array.map(fun s-> toInt32 s)
            |>Array.choose id
        intArray

    let toExternalDetection (xElement:XElement) =
        match(result{
            let! returnCodesString = getRequiredAttribute xElement "rc"
            let commandLine = xElement.Value
            let returnCodes = toIntArray returnCodesString
            let externalDetection =                    
                        {
                            LsupEval.ExternalDetection.ExternalDetection.CommandLine = commandLine
                            LsupEval.ExternalDetection.ExternalDetection.ReturnCodes = returnCodes
                        }
            return externalDetection
        })with
        |Result.Ok ed -> ApplicabilityRule.ExternalDetection ed
        |Result.Error ex -> raise ex

    //Example:
    //<_Coreq name="PMD_WIN10X64">
    //   <Version>0.1^</Version>
    //</_Coreq>
    let toCoreq (xElement:XElement) =
        match(result{
            let! name = getRequiredAttribute xElement "name"
            let versionElement = getOptionalChildXElement xElement "Version"
            let levelXElement = getOptionalChildXElement xElement  "Level"
            let coreq : Coreq.CoreqElement =                  
                    {
                        Name = name
                        Version =                             
                                match versionElement with
                                |Some v -> LsupEval.Driver.VersionOrLevelElement.VersionElement v.Value
                                |None ->
                                    match levelXElement with
                                    |Some l -> LsupEval.Driver.VersionOrLevelElement.LevelElement l.Value
                                    |None-> raise (toException "_Coreq element must contain either a Version element or a Level element." None)
                    }
            return coreq
        })with
        |Result.Ok cr -> ApplicabilityRule.Coreq cr
        |Result.Error ex -> raise ex
        
    //Check if xml string is an xml element (rooted)
    let isXmlRooted xmlString = 
        try
            XElement.Parse(xmlString) |> ignore
            true
        with
        | _ -> 
            false

    let toValidApplicabilityRule (applicabilityXml:string) =
        if (isXmlRooted applicabilityXml) then
            applicabilityXml
        else
            sprintf "<And>%s</And>" applicabilityXml

    let rec lsupXmlToApplicabilityRules (logger:Common.Logging.ILog) (applicabilityXml:string) : ApplicabilityRule =
        let nameTable = new NameTable()
        let namespaceManager = new XmlNamespaceManager(nameTable);
        let xmlParserContext = XmlParserContext(null,namespaceManager,null,XmlSpace.None)
        let normalizedApplicabilityXml = toValidApplicabilityRule applicabilityXml
        use xmlReader = new XmlTextReader(normalizedApplicabilityXml,XmlNodeType.Element,xmlParserContext)
        let xElement = XElement.Load(xmlReader)
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Processing ApplicabilityRule element: %s" xElement.Name.LocalName))|>ignore))
        let applicabilityRules =
            match xElement.Name.LocalName with
            |"_Bios" -> (toBios xElement)
            |"_CPUAddressWidth" -> (toCpuAddressWidth xElement)
            |"_OS" -> (toOperatingSystem xElement)
            |"_OSLang" -> (toOsLanguage xElement)
            |"_Driver" -> (toDriver xElement)
            |"_EmbeddedControllerVersion" -> (toEmbeddedControllerVersion xElement)
            |"_FileExists" -> (toFileExists  xElement)
            |"_FileVersion" -> (toFileVersion xElement)
            |"_RegistryKey" -> (toRegistryKey xElement)
            |"_PnPID" -> (toPnPId xElement)
            |"_RegistryKeyValue" -> (toRegistryKeyValue xElement)
            |"_ExternalDetection" -> (toExternalDetection xElement)
            |"_Coreq" -> (toCoreq xElement)
            |"True" -> ApplicabilityRule.True
            |"False" -> ApplicabilityRule.False
            |"And" -> 
                ApplicabilityRule.And (
                    xElement.Elements()
                    |>Seq.map (fun x -> (                                        
                                            lsupXmlToApplicabilityRules logger (x.ToString()))
                                )
                    |>Seq.toArray
                    )
            |"Or" -> 
                ApplicabilityRule.Or (    
                    xElement.Elements()
                    |>Seq.map (fun x -> (lsupXmlToApplicabilityRules logger (x.ToString())))
                    |>Seq.toArray 
                    )
            |"Not" -> 
                ApplicabilityRule.Not (lsupXmlToApplicabilityRules logger ((xElement.Descendants()|>Seq.head).ToString()))
            |_ -> raise (new NotSupportedException(sprintf "Applicability rule for '%s' is not implemented." xElement.Name.LocalName))
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Lsup converted to ApplicabilityRule: %A" applicabilityRules))|>ignore))
        applicabilityRules

    let loadLsuPackageFromString xmlString : Result<LsuPackage,Exception> =
        result{
            let! xDocument = loadLsuPackageXDocumentFromString xmlString
            let! xElement = loadLsuPackageXElement xDocument
            let! lsuPackage = loadLsuPackage xElement
            return lsuPackage
        }

    let loadLsuPackageFromFile file : Result<LsuPackage,Exception> =
        result{
            let! xDocument = loadLsuPackageXDocument file
            let! xElement = loadLsuPackageXElement xDocument
            let! lsuPackage = loadLsuPackage xElement
            return lsuPackage
        }

    let loadLsuPackagesFromFiles files  : Result<seq<LsuPackage>,Exception> =
        let lsuPackageResults =
            files
            |> Array.map loadLsuPackageFromFile
            |> Array.toSeq
            |>toAccumulatedResult
        lsuPackageResults

        