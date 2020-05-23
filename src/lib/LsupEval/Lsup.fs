namespace LsupEval

module Lsup =
    open System
    open System.Xml.Linq

    type LsuRebootType = Default=0|RebootForced=1|Reserved=2|RebootRequired=3|Shutdown=4|RebootDelayed=5
        
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
                }                
        }


