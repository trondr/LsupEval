namespace LsupEval.Tests

module LsupTest =
    
    open NUnit.Framework
    open LsupEval;
    open LsupEval.LsupData
    open LsupEval.Logging
    let logger = LsupEval.Logging.getLoggerByName "Rules"
    
    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``loadLsuPackageXDocument xml file exists return OK`` () =
        let existingUpdateFile = System.IO.Path.Combine(UpdatesTestData.UpdateFilesFolder,"nz3gs07w_2_.xml")
        let actual = Lsup.loadLsuPackageXDocument existingUpdateFile
        match actual with
        |Result.Ok _ -> Assert.IsTrue(true)
        |Result.Error ex -> Assert.Fail(ex.Message)


    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``loadLsuPackageXDocument xml file does not exists return Error`` () =
        let nonExistingUpdateFile = System.IO.Path.Combine(UpdatesTestData.UpdateFilesFolder,"nz3gs07w_2_DoesNotExist.xml")
        let actual = Lsup.loadLsuPackageXDocument nonExistingUpdateFile
        match actual with
        |Result.Ok _ -> Assert.IsTrue(false, "Expected error result")
        |Result.Error ex -> 
            Assert.IsTrue(ex.Message.Contains("Failed to load"))
            Assert.IsTrue(ex.Message.Contains("Could not find file"))
            Assert.IsTrue(ex.Message.Contains("nz3gs07w_2_DoesNotExist.xml"),"Error message does not contain file name.")

    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``loadLsuPackage test`` () =
        match(result{
            let! xDocument = Lsup.loadLsuPackageXDocument @"E:\Dev\github.trondr\LsupEval\src\test\LsupEval.Tests\TestData\nz3gs07w_2_.xml"
            let! lsuPackageXElement = Lsup.loadLsuPackageXElement xDocument
            let! lsuPackage = Lsup.loadLsuPackage lsuPackageXElement
            return lsuPackage
        })with
        |Result.Ok p ->         
            Assert.AreEqual("nz3gs07w",p.Id,"Package.Id")
            Assert.AreEqual("ISDAS_NZ3GS",p.Name,"Package.Name")
            Assert.AreEqual("2.7.100.2",p.Version,"Package.Version")
            Assert.AreEqual(false,p.Hide,"Package.Hide")
            Assert.AreEqual(LsupData.LsuRebootType.RebootRequired,p.RebootType,"Package.RebootType")
            Assert.AreEqual("Intel® SGX Device and Software (Windows 10 Version 1709 or later) - 10 [64]",p.Title,"Package.Title")
            match p.DetectInstall with
            |None -> Assert.Fail("DetectInstall is None")
            |Some d -> Assert.IsTrue(d.Contains("VEN_INT&DEV_0E0C"))

            match p.Dependencies with
            |None -> Assert.Fail("Dependencies is None")
            |Some d -> Assert.IsTrue(d.Contains("VEN_INT&DEV_0E0C"))

            Assert.AreEqual(1,p.Files.Installer.Files.Length,"Installer Files Length")
            Assert.AreEqual("nz3gs07w.exe",p.Files.Installer.Files.[0].Name,"Installer Name")
            Assert.AreEqual("82AA3021D239C103D6443311284CAA84665F1478C4CD00F3BB62576D986C324B",p.Files.Installer.Files.[0].Crc,"Installer Crc")
            Assert.AreEqual(4952256,p.Files.Installer.Files.[0].Size,"Installer Size")

            match(p.Files.External) with
            |Some e -> 
                Assert.AreEqual(1,e.Files.Length,"External Files Length")
                Assert.AreEqual("getw10ver7.exe",e.Files.[0].Name,"External Name")
                Assert.AreEqual("19F4E41194B3FA30493BBFBCC7D20C7D940506DAF256FB662EEDC25FB047CDC2",e.Files.[0].Crc,"External Crc")
                Assert.AreEqual(159528,e.Files.[0].Size,"External Size")
            |None -> Assert.Fail("External files expected.")

            match(p.Files.Readme) with
            |Some r -> 
                Assert.AreEqual(1,r.Files.Length,"External Files Length")
                Assert.AreEqual("EN",r.Default,"Readme Id")                
                Assert.AreEqual("nz3gs07w.txt",r.Files.[0].Name,"External Name")
                Assert.AreEqual("9DDFD6C10B149F32BFB369022CBF3AD6C5B20425753D3AF8A34F0453C68DA257",r.Files.[0].Crc,"External Crc")
                Assert.AreEqual(22152,r.Files.[0].Size,"External Size")
            |None -> Assert.Fail("Readme file expected.")

            Assert.IsTrue(p.Files.External.IsSome,"External is missing")
        |Result.Error ex -> Assert.Fail(ex.Message)
  
    open LsupEval.Rules
    open LsupEval.Bios

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isBiosMatchTest star -> return true``  () =
        let biosVersion = "R06ET3SDE"
        let bios = {Versions = [|"*";|]}
        let actual = isBiosMatch logger bios biosVersion
        Assert.AreEqual(true,actual)        
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isBiosMatchTest star and some non matching -> return true``  () =
        let biosVersion = "R06ET3SDE"
        let bios = {Versions = [|"*";"R07ET*";"R08ET*";"N1HET*";"R05ET*";"N1DET*";"N1EET*";"N1KET*";"N1LET*";"R02ET*";"N1CET*";"N1GET*";"N1FET*";|]}
        let actual = isBiosMatch logger bios biosVersion
        Assert.AreEqual(true,actual)        
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isBiosMatchTest non matching -> return false``  () =
        let biosVersion = "R06ET3SDE"
        let bios = {Versions = [|"R07ET*";"R08ET*";"N1HET*";"R05ET*";"N1DET*";"N1EET*";"N1KET*";"N1LET*";"R02ET*";"N1CET*";"N1GET*";"N1FET*";|]}
        let actual = isBiosMatch logger bios biosVersion
        Assert.AreEqual(false,actual)
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isBiosMatchTest matching -> return true``  () =
        let biosVersion = "N1FET123456"
        let bios = {Versions = [|"R06ET*";"R07ET*";"R08ET*";"N1HET*";"R05ET*";"N1DET*";"N1EET*";"N1KET*";"N1LET*";"R02ET*";"N1CET*";"N1GET*";"N1FET*";|]}
        let actual = isBiosMatch logger bios biosVersion
        Assert.AreEqual(true,actual)        
        ()

    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``getCurrentBiosVersion a machine specific test on Lenovo ThinkPad P50 on 2020-05-24`` () =
        match(result{
            let! actual = getCurrentBiosVersion()
            Assert.AreEqual("N1EET87W (1.60 )",actual,"BIOS version")
            return actual
        })with
        |Result.Ok _ -> Assert.IsTrue(true)
        |Result.Error ex -> Assert.Fail(ex.Message)  
        
    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``getCurrentCpuAddressWidth a machine specific test on Lenovo ThinkPad P50 on 2020-05-24`` () =
        match(result{
            let! actual = Cpu.getCurrentCpuAddressWidth()
            Assert.AreEqual(Cpu.CpuAddressWidth.Bit64,actual,"Cpu address width")
            return actual
        })with
        |Result.Ok _ -> Assert.IsTrue(true)
        |Result.Error ex -> Assert.Fail(ex.Message)  

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isCpuAddressWidthMatch matching -> return true``  () =        
        let requiredCpuAddressWidth = Cpu.CpuAddressWidth.Bit64
        let currentCpuAddressWidth = Cpu.CpuAddressWidth.Bit64        
        let actual = Cpu.isCpuAddressWidthMatch logger requiredCpuAddressWidth currentCpuAddressWidth
        Assert.AreEqual(true,actual)        
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isCpuAddressWidthMatch matching 2 -> return true``  () =        
        let requiredCpuAddressWidth = Cpu.CpuAddressWidth.Bit32
        let currentCpuAddressWidth = Cpu.CpuAddressWidth.Bit32        
        let actual = Cpu.isCpuAddressWidthMatch logger requiredCpuAddressWidth currentCpuAddressWidth
        Assert.AreEqual(true,actual)        
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isCpuAddressWidthMatch non matching -> return true``  () =        
        let requiredCpuAddressWidth = Cpu.CpuAddressWidth.Bit64
        let currentCpuAddressWidth = Cpu.CpuAddressWidth.Bit32        
        let actual = Cpu.isCpuAddressWidthMatch logger requiredCpuAddressWidth currentCpuAddressWidth
        Assert.AreEqual(false,actual)
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isCpuAddressWidthMatch non matching 2-> return true``  () =        
        let requiredCpuAddressWidth = Cpu.CpuAddressWidth.Bit32
        let currentCpuAddressWidth = Cpu.CpuAddressWidth.Bit64        
        let actual = Cpu.isCpuAddressWidthMatch logger requiredCpuAddressWidth currentCpuAddressWidth
        Assert.AreEqual(false,actual)
        ()

    let workingDirectory = "c:\\windows\\temp"
    let applicabiliyRules = """
<And>
    <_Bios>
        <Level>N1XET*</Level>
    </_Bios>
    <And>      
        <_CPUAddressWidth>
            <AddressWidth>64</AddressWidth>
        </_CPUAddressWidth>
        <_OS>
            <OS>WIN10</OS>
            <OS>WIN10.*</OS>
            <OS>WIN10-ENT</OS>
            <OS>WIN10-ENT.*</OS>
            <OS>WIN10-PRO</OS>
            <OS>WIN10-PRO.*</OS>
        </_OS>
        <_Driver>
            <HardwareID><![CDATA[PCI\VEN_1022&DEV_1537]]></HardwareID>
            <HardwareID><![CDATA[PCI\VEN_1022&DEV_1578]]></HardwareID>
            <HardwareID><![CDATA[PCI\VEN_1022&DEV_1456]]></HardwareID>
            <HardwareID><![CDATA[PCI\VEN_1022&DEV_15DF]]></HardwareID>
            <HardwareID><![CDATA[PCI\VEN_1022&DEV_1486]]></HardwareID>
            <Date>2019-05-21</Date>
            <Version>4.10.0.0^</Version>
        </_Driver>
    </And>        
</And>        
          """
    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Non-Matching`` () =
        let systemInformationFalse = { 
                BiosVersion = "XYZ"
                CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
                Os = "WIN10"
                OsLang="JP"
                Drivers= [||]
                EmbeddedControllerVersion="1.17"
                PnPIds = [||]
            }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules
        printf "%A" applicabilityRule
        let actual = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationFalse workingDirectory None applicabilityRule 
        let expecedFalse = false
        Assert.AreEqual(expecedFalse,actual,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationFalse)


    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware but is Non-Matching on driver version`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= 
                [|
                    Driver.DriverInfo.Hardware
                        {
                            HardwareIds=Some [|"PCI\VEN_1022&DEV_1537"|]
                            CompatibleIds = [||]
                            Name="Test Name"
                            Date=(new System.DateTime(2019,05,21))
                            Version= (Driver.Version "4.8.0.0")
                            ProviderName="Lenovo"
                        }
                |]
            EmbeddedControllerVersion="1.17"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on driver version but is not matching on date`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= 
                [|
                    Driver.DriverInfo.Hardware
                        {
                            HardwareIds=Some [|"PCI\VEN_1022&DEV_1537"|]
                            CompatibleIds = [||]
                            Name="Test Name"
                            Date=(new System.DateTime(2019,05,21))
                            Version= (Driver.Version "4.10.0.0")
                            ProviderName="Lenovo"
                        }
                |]
            EmbeddedControllerVersion="1.17"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on driver version and is matching on date`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= 
                [|
                    Driver.DriverInfo.Hardware
                        {
                            HardwareIds=Some [|"PCI\VEN_1022&DEV_1537"|]
                            CompatibleIds = [||]
                            Name="Test Name"
                            Date=(new System.DateTime(2019,04,21))
                            Version= (Driver.Version "4.10.0.0")
                            ProviderName="Lenovo"
                        }
                |]
            EmbeddedControllerVersion="1.17"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRules2 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <And>      
            <_CPUAddressWidth>
                <AddressWidth>64</AddressWidth>
            </_CPUAddressWidth>
            <_OS>
                <OS>WIN10</OS>
                <OS>WIN10.*</OS>
                <OS>WIN10-ENT</OS>
                <OS>WIN10-ENT.*</OS>
                <OS>WIN10-PRO</OS>
                <OS>WIN10-PRO.*</OS>
            </_OS>
            <_Driver>
                <File>%Windows%\System32\drivers\ibtusb.sys</File>
                <Date>2019-05-21</Date>
                <Version>4.10.0.0^</Version>
            </_Driver>
        </And>        
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on driver file version and is matching on date`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= 
                [|                   
                    Driver.DriverInfo.File
                        {
                            FilePath="C:\\Windows\\System32\\drivers\\ibtusb.sys"
                            Date=(new System.DateTime(2019,04,21))
                            Version="4.10.0.0"                            
                        }
                |]
            EmbeddedControllerVersion="1.17"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules2
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRules3 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_EmbeddedControllerVersion>
            <Version>1.20^</Version>
        </_EmbeddedControllerVersion>        
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on Embedded Controller version`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules3
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Non-Matching on Embedded Controller version`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.17"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules3
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesFileExists = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_FileExists>%WINDOWS%\notepad.exe</_FileExists>        
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on file exists`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesFileExists
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesFileExists2 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_FileExists>%WINDOWS%\notepad32.exe</_FileExists>        
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is not Matching on file exists`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesFileExists2
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesFileVersion = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_FileVersion>
        <File>%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe</File>
        <Version>6.2.0.1239^</Version>
    </_FileVersion>        
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on file version`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesFileVersion
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesFileVersion2 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_FileVersion>
        <File>%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe</File>
        <Version>90.2.0.1239^</Version>
    </_FileVersion>        
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is not Matching on file version`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesFileVersion2
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesOsLang = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_OSLang>
            <Lang>JP</Lang>
        </_OSLang>        
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on OS lang`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"      
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesOsLang
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesOsLang2 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_OSLang>
            <Lang>JP</Lang>
        </_OSLang>        
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is non-matching on OS lang`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="NO"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"   
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesOsLang2
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesRegistryKeyExist = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_RegistryKey>
            <Key>HKEY_LOCAL_MACHINE\Software_Non</Key>
            <Key>HKEY_LOCAL_MACHINE\Software</Key>
        </_RegistryKey>
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on Registry exists`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"        
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesRegistryKeyExist
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesRegistryKeyExist2 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <_RegistryKey>
            <Key>HKEY_LOCAL_MACHINE\Software_Non</Key>
            <Key>HKEY_LOCAL_MACHINE\Software_Non2</Key>
        </_RegistryKey>
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is non-matching on registry key exists`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="NO"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [||]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesRegistryKeyExist2
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()


    let applicabiliyRulesPnPIds = """
       <And>
           <_Bios>
               <Level>N1XET*</Level>
           </_Bios>
           <Or>
               <_PnPID><![CDATA[PCI\VEN_8086&DEV_2826&CC_0104]]></_PnPID>
               <_PnPID><![CDATA[PCI\VEN_8086&DEV_2826]]></_PnPID>
               <_PnPID><![CDATA[PCI\VEN_8086&DEV_201D]]></_PnPID>
            </Or>
       </And>        
                 """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on pnp ids exists`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"        
            PnPIds = [|"PCI\VEN_8086&DEV_2826&CC_0104";"PCI\VEN_8086&DEV_2826";"PCI\VEN_8086&DEV_201D"|]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesPnPIds
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesPnPIds2 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <Or>
            <_PnPID><![CDATA[PCI\VEN_8086&DEV_2826&CC_0104]]></_PnPID>            
        </Or>
    </And>        
                """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is non-matching on pnp ids exists`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="NO"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [|"PCI\VEN_8086&DEV_2826";"PCI\VEN_8086&DEV_201D"|]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesPnPIds2
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesRegistryKeyValue = """
       <And>
           <_Bios>
               <Level>N1XET*</Level>
           </_Bios>
           <Or>
               <_RegistryKeyValue type="REG_SZ">
                   <Key>HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\nvlddmkm1234</Key>
                   <KeyName>DCHUVen</KeyName>
                   <Version>1^</Version>
               </_RegistryKeyValue>
               <_RegistryKeyValue type="REG_SZ">
                   <Key>HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\ASP.NET</Key>
                   <KeyName>RootVer</KeyName>
                   <Version>4.0.30319.0^</Version>
               </_RegistryKeyValue>
            </Or>
       </And>        
                 """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on registry key value`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="JP"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"        
            PnPIds = [|"PCI\VEN_8086&DEV_2826&CC_0104";"PCI\VEN_8086&DEV_2826";"PCI\VEN_8086&DEV_201D"|]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesRegistryKeyValue
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesRegistryKeyValue2 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <Or>
            <_RegistryKeyValue type="REG_SZ">
                <Key>HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\nvlddmkm1234</Key>
                <KeyName>DCHUVen</KeyName>
                <Version>1^</Version>
            </_RegistryKeyValue>
            <_RegistryKeyValue type="REG_SZ">
                <Key>HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\ASP.NET</Key>
                <KeyName>RootVer</KeyName>
                <Version>^3.0.0.0</Version>
            </_RegistryKeyValue>
        </Or>
    </And>        
                """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is non-matching on registry key value`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="NO"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [|"PCI\VEN_8086&DEV_2826";"PCI\VEN_8086&DEV_201D"|]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesRegistryKeyValue2
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesRegistryKeyValue3 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <Or>
            <_RegistryKeyValue type="REG_SZ">
                <Key>HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\nvlddmkm1234</Key>
                <KeyName>DCHUVen</KeyName>
                <Version>1^</Version>
            </_RegistryKeyValue>
            <_RegistryKeyValue type="REG_SZ">
                <Key>HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\ASP.NET</Key>
                <KeyName>RootVer</KeyName>
                <Level>4.*.0</Level>
            </_RegistryKeyValue>
        </Or>
    </And>        
                """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is matching on registry key value with level`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="NO"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [|"PCI\VEN_8086&DEV_2826";"PCI\VEN_8086&DEV_201D"|]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesRegistryKeyValue3
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = true
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()

    let applicabiliyRulesRegistryKeyValue4 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <Or>
            <_RegistryKeyValue type="REG_SZ">
                <Key>HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\nvlddmkm1234</Key>
                <KeyName>DCHUVen</KeyName>
                <Version>1^</Version>
            </_RegistryKeyValue>
            <_RegistryKeyValue type="REG_SZ">
                <Key>HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\ASP.NET</Key>
                <KeyName>RootVer</KeyName>
                <Level>5.*.0</Level>
            </_RegistryKeyValue>
        </Or>
    </And>        
                """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is non-matching on registry key value with level`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            OsLang="NO"
            Drivers= [||]
            EmbeddedControllerVersion="1.21"
            PnPIds = [|"PCI\VEN_8086&DEV_2826";"PCI\VEN_8086&DEV_201D"|]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesRegistryKeyValue4
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
        ()


    let applicabiliyRulesExternalDetection = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <Or>
            <_ExternalDetection rc="3010">cmd.exe /c "exit 3010"</_ExternalDetection>
            <_ExternalDetection rc="5">cmd.exe /c "exit 2"</_ExternalDetection>
         </Or>
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is Matching on external detection`` () =
     let systemInformationTrue = { 
         BiosVersion = "N1XET1234567"
         CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
         Os = "WIN10"
         OsLang="JP"
         Drivers= [||]
         EmbeddedControllerVersion="1.21"        
         PnPIds = [|"PCI\VEN_8086&DEV_2826&CC_0104";"PCI\VEN_8086&DEV_2826";"PCI\VEN_8086&DEV_201D"|]
     }
     let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesExternalDetection
     let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
     let expectedTrue = true
     Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
     ()

    let applicabiliyRulesExternalDetection2 = """
    <And>
        <_Bios>
            <Level>N1XET*</Level>
        </_Bios>
        <Or>
            <_ExternalDetection rc="4">cmd.exe /c "exit 3010"</_ExternalDetection>
            <_ExternalDetection rc="5">cmd.exe /c "exit 2"</_ExternalDetection>
         </Or>
    </And>        
              """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware and is non matching on external detection`` () =
     let systemInformationTrue = { 
         BiosVersion = "N1XET1234567"
         CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
         Os = "WIN10"
         OsLang="JP"
         Drivers= [||]
         EmbeddedControllerVersion="1.21"        
         PnPIds = [|"PCI\VEN_8086&DEV_2826&CC_0104";"PCI\VEN_8086&DEV_2826";"PCI\VEN_8086&DEV_201D"|]
     }
     let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRulesExternalDetection2
     let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue workingDirectory None applicabilityRule 
     let expectedTrue = false
     Assert.AreEqual(expectedTrue,actual2,sprintf "Evaluation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
     ()

    [<Test>]
    [<Timeout(2000000)>]
    [<Category(TestCategory.ManualTests)>]
    let ``getPnpDriversTests ``()=
        match(Driver.getPnpDrivers())with
        |Result.Ok actual ->
            printf "%A" actual
            ()
        |Result.Error ex -> 
            Assert.Fail(ex.ToString())


    [<Test>]
    [<Timeout(2000000)>]
    [<Category(TestCategory.ManualTests)>]
    let ``getPnpDevicePropertiesTests ``()=
        match(Driver.getPnpDeviceProperties())with
        |Result.Ok actual ->
            printf "%A" actual
            ()
        |Result.Error ex -> 
            Assert.Fail(ex.ToString())
    
    
    [<Test>]
    [<Category(TestCategory.UnitTests)>]
        [<TestCase("Valid version 1 -> true","1",true)>]
        [<TestCase("Valid version 1.0 -> true","1.0",true)>]
        [<TestCase("Valid version 1.0.0 -> true","1.0.0",true)>]
        [<TestCase("Valid version 1.0.0.0 -> true","1.0.0.0",true)>]
        [<TestCase("Valid version 1.0.0.0.0 -> true","1.0.0.0.0",true)>]
        [<TestCase("Invalid version '' -> true","",false)>]
        [<TestCase("Invalid version null -> true",null,false)>]
        [<TestCase("Invalid version 1.0.0. -> true","1.0.0.",false)>]
        [<TestCase("Invalid version 1.0..0 -> true","1.0.0.",false)>]
        [<TestCase("Invalid version ^1.0..0 -> true","^1.0.0.",false)>]

    let ``version `` (description:string,version:string,expectedSuccess:bool) =
        match(result{
            let! actual = LsupEval.Version.version version
            return actual
        })with
        |Result.Ok v -> Assert.IsTrue(expectedSuccess,"Did not expect success")
        |Result.Error ex -> Assert.False(expectedSuccess,"Did expect success" + ex.ToString())

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    [<TestCase("1 tupple numeric -> Equal (0)","1","1",0)>]
    [<TestCase("1 tupple numeric -> Greater Than (1)","2","1",1)>]
    [<TestCase("1 tupple numeric -> Less Than (-1)","1","2",-1)>]

    [<TestCase("2 tupple numeric -> Equal (0)","1.0","1.0",0)>]
    [<TestCase("2 tupple numeric -> Greater Than (1)","2.0","1.0",1)>]
    [<TestCase("2 tupple numeric -> Less Than (-1)","1.0","2.0",-1)>]

    [<TestCase("3 tupple numeric -> Equal (0)","1.0.0","1.0.0",0)>]
    [<TestCase("3 tupple numeric -> Greater Than (1)","2.0.0","1.0.0",1)>]
    [<TestCase("3 tupple numeric -> Less Than (-1)","1.0.0","2.0.0",-1)>]

    [<TestCase("4 tupple numeric -> Equal (0)","2.0.0.1","2.0.0.1",0)>]
    [<TestCase("4 tupple numeric -> Greater Than (1)","2.0.0.1","2.0.0.0",1)>]
    [<TestCase("4 tupple numeric -> Less Than (-1)","2.0.0.0","2.0.0.1",-1)>]

    [<TestCase("4 tupple numeric and alphanumeric -> Equal (0)","1.0.0.1-alpha","1.0.0.1-alpha",0)>]
    [<TestCase("4 tupple numeric and alphanumeric -> Greater Than (1)","2.0.0.1-alpha","2.0.0.1",1)>]
    [<TestCase("4 tupple numeric and alphanumeric -> Less Than (-1)","2.0.0.0","2.0.0.0-alpha",-1)>]

    [<TestCase("4 tupple numeric and alphanumeric -> Equal (0)","1.0.0.1-beta","1.0.0.1-beta",0)>]
    [<TestCase("4 tupple numeric and alphanumeric -> Greater Than (1)","2.0.0.1-beta","2.0.0.1",1)>]
    [<TestCase("4 tupple numeric and alphanumeric -> Greater Than (1)","2.0.0.1-beta","2.0.0.1-alpha",1)>]
    [<TestCase("4 tupple numeric and alphanumeric -> Less Than (-1)","2.0.0.0","2.0.0.1-alpha",-1)>]
    [<TestCase("4 tupple numeric and alphanumeric -> Less Than (-1)","2.0.0.0-alpha","2.0.0.1-beta",-1)>]

    [<TestCase("2 and 4 tupple numeric and alphanumeric -> Less Than (0)","2.0","2.0.0.1-beta",0)>]
    [<TestCase("4 and 2 tupple numeric and alphanumeric -> Less Than (0)","2.0.0.1-beta","2.0",0)>]

    let ``compare Version tests `` (description:string,version1:string,version2:string, expected:int32) =
        match(result{
            let! lsupVersion1 = LsupEval.Version.version version1
            let! lsupVersion2 = LsupEval.Version.version version2
            let res = LsupEval.Version.compare lsupVersion1 lsupVersion2
            return res
        })with
        |Result.Ok v -> 
            Assert.AreEqual(expected,v)
        |Result.Error ex ->
            Assert.Fail(ex.ToString())

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    [<TestCase("Char 0  -> true",'0',true)>]
    [<TestCase("Char 1  -> true",'1',true)>]
    [<TestCase("Char 2  -> true",'2',true)>]
    [<TestCase("Char 3  -> true",'3',true)>]
    [<TestCase("Char 4  -> true",'4',true)>]
    [<TestCase("Char 5  -> true",'5',true)>]
    [<TestCase("Char 6  -> true",'6',true)>]
    [<TestCase("Char 7  -> true",'7',true)>]
    [<TestCase("Char 8  -> true",'8',true)>]
    [<TestCase("Char 9  -> true",'9',true)>]
    [<TestCase("Char a  -> false",'a',false)>]
    [<TestCase("Char z  -> false",'z',false)>]
    [<TestCase("Char ''  -> false","",false)>]
    let `` isNumericChar Tests`` (description:string,ch:char, expected:bool) =
        let actual = LsupEval.Version.isNumericChar ch
        Assert.AreEqual(expected,actual,description)

    [<Test>]
    [<Category(TestCategory.UnitTests)>]    
    [<TestCase("Valid version pattern ^1.0.0 -> true","^1.0.0",true)>]
    [<TestCase("Valid version pattern ^1.0.0 -> true","^1.0.0^",false)>]
    [<TestCase("Valid version pattern ^1.0.0 -> true","1.0.0^",true)>]
    [<TestCase("Valid version pattern ^1.0.0 -> true","1.0.0",true)>]
    let ``versionPattern test`` (description:string,versionPattern:string,expectedSuccess:bool) =
        match(result{
            let! actual = LsupEval.Version.versionPattern versionPattern
            return actual
        })with
        |Result.Ok v -> Assert.IsTrue(expectedSuccess,"Did not expect success")
        |Result.Error ex -> Assert.False(expectedSuccess,"Did expect success" + ex.ToString())


    [<Test>]
    [<Category(TestCategory.UnitTests)>]    
    [<TestCase("LowerOrEqual - No match","^1.0.0","2.0.0",false,true)>]
    [<TestCase("LowerOrEqual - match","^1.0.0","1.0.0",true,true)>]
    [<TestCase("LowerOrEqual - match","^1.0.0","0.2.0",true,true)>]

    [<TestCase("HigherOrEqual - match","1.0.0^","2.0.0",true,true)>]
    [<TestCase("HigherOrEqual - match","1.0.0^","1.0.0",true,true)>]
    [<TestCase("HigherOrEqual - match","1.0.0^","0.2.0",false,true)>]

    [<TestCase("Equal - match","1.0.0","2.0.0",false,true)>]
    [<TestCase("Equal - match","1.0.0","1.0.0",true,true)>]
    [<TestCase("Equal - match","1.0.0","0.2.0",false,true)>]

    let ``isVersionPatternMatch test`` (description:string,versionPattern:string,version:string,expected:bool, epectedSuccess:bool) =
        match(result{
            let! version = LsupEval.Version.version version
            let! versionPattern = LsupEval.Version.versionPattern versionPattern
            let actual = LsupEval.Version.isVersionPatternMatch version versionPattern
            Assert.AreEqual(expected,actual,"Version match not expected.")
            return actual
        })with
        |Result.Ok v -> Assert.IsTrue(epectedSuccess,"Did not expect success")
        |Result.Error ex -> Assert.False(epectedSuccess,"Did expect success" + ex.ToString())

    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``getEmbeddedControllerVersion test`` () =
        match(result{
            let! actual = LsupEval.EmbeddedController.getCurrentEmbeddedControllerVersion()        
            Assert.IsTrue(not (System.String.IsNullOrEmpty(actual)),"Embedded Controller version is null")
            return actual
        })with
        |Result.Ok v -> Assert.IsTrue(true,"Did not expect success")
        |Result.Error ex -> Assert.IsTrue(false,"Did expect success" + ex.ToString())

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    [<TestCase("Valid","EN",true)>]
    [<TestCase("Valid","En",true)>]
    [<TestCase("Valid","fr",true)>]
    [<TestCase("Valid","iT",true)>]
    [<TestCase("Valid","DE",true)>]
    [<TestCase("Valid","ES",true)>]
    [<TestCase("Valid","NO",true)>]
    [<TestCase("Valid","SV",true)>]
    [<TestCase("Valid","Fi",true)>]
    [<TestCase("Valid","Da",true)>]
    [<TestCase("Valid","NL",true)>]
    [<TestCase("Valid","Ptb",true)>]
    [<TestCase("Valid","AR",true)>]
    [<TestCase("Valid","CS",true)>]
    [<TestCase("Valid","EL",true)>]
    [<TestCase("Valid","HE",true)>]
    [<TestCase("Valid","HU",true)>]
    [<TestCase("Valid","PL",true)>]
    [<TestCase("Valid","RU",true)>]
    [<TestCase("Valid","Tr",true)>]
    [<TestCase("Valid","JP",true)>]
    [<TestCase("Valid","KO",true)>]
    [<TestCase("Valid","CHs",true)>]
    [<TestCase("Valid","CHt",true)>]
    [<TestCase("Valid","*",true)>]
    [<TestCase("Valid","XYZ",false)>]
    let ``language tests`` (description:string,languageCodeString:string,expectedSuccess:bool) =
        match(result{
            let! actual = LsupEval.Language.language languageCodeString
            let testAssert =
                if(expectedSuccess) then
                    Assert.AreEqual(languageCodeString,actual.LanguageCode,sprintf "Language code %A" actual)
            return actual
        })with
        |Result.Ok v -> Assert.IsTrue(expectedSuccess,"Did not expect success")
        |Result.Error ex -> Assert.IsFalse(expectedSuccess,"Did expect success" + ex.ToString())
        

        ()
        
    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let getSystemInformationTest () =
        let testResult = result{
                let! systemInformation = LsupEval.Rules.getCurrentSystemInformation()
                return systemInformation
            }
        match testResult with        
        |Result.Ok v -> Assert.IsTrue(not (System.String.IsNullOrWhiteSpace(v.BiosVersion)),"Did not expect success")
        |Result.Error ex -> Assert.Fail("Did expect success" + ex.ToString())


    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let getSystemInformation'Test () =
        let testResult = result{
                let! systemInformation = LsupEval.Rules.getCurrentSystemInformation'()
                return systemInformation
            }
        match testResult with        
        |Result.Ok v -> Assert.IsTrue(not (System.String.IsNullOrWhiteSpace(v.BiosVersion)),"Did not expect success")
        |Result.Error ex -> Assert.Fail("Did expect success" + ex.ToString())

    open LsupEval.Registry
    type internal RegistryTestData ={ValueName:string;Value:string;PatternValueName:string;PatternValue:string;IsMatch:bool}
    let testKey = {Hive=Microsoft.Win32.Registry.LocalMachine;SubKeyPath="SOFTWARE\\WOW6432Node\\Intel\\ME"}
    let internal registryTestData =        
        [|
            yield {ValueName="MEIVersion"; Value="";PatternValueName="MEIVersion";PatternValue="11.7.0.1040^";IsMatch=false}
            yield {ValueName="MEIVersion"; Value="11.7.0.1040";PatternValueName="MEIVersion";PatternValue="11.7.0.1040^";IsMatch=true}
        |]

    [<Test>]
    [<TestCaseSource("registryTestData")>]
    [<Category(TestCategory.UnitTests)>]
    let isRegistryKeyValueMatchTest (testDataObject:obj) =
        let testData = (testDataObject:?>RegistryTestData)        
        let registryKeyValue =
            let valueData = Some { Value=testData.Value; ValueKind=RegValueKind.RegSz}
            LsupEval.Registry.toRegistryKeyValue testKey testData.ValueName valueData
        let registryKeyValuePattern =
            {
                Key = testKey
                ValueName = testData.PatternValueName
                ValueKind = RegValueKind.RegSz
                Value = LsupEval.Registry.ValuePattern.Version testData.PatternValue
            }
        let actualIsMatch = LsupEval.Registry.isRegistryKeyValueMatch logger registryKeyValuePattern registryKeyValue
        Assert.AreEqual(testData.IsMatch,actualIsMatch,"Registry value match was not as expected.")
        ()    

    let testUpdate1 = """<?xml version="1.0" encoding="utf-8"?>
    <?pcdDescriptor version="0230"?>
    <Package name="PMD_N2HKU_32_SKL" id="n2hku07w_x86_skl" version="1.67.16.42" hide="False">
      <Title default="EN">
        <Desc id="EN">Lenovo Power Management Driver - 7/8/8.1/10 [32]</Desc>
      </Title>
      <ReleaseDate>2019-12-18</ReleaseDate>
        <Reboot type="3" />  
      <Install rc="0,1,1073741825" type="cmd" default="EN">
        <Cmdline id="EN">%PACKAGEPATH%\dpinst_x86.exe /s</Cmdline>
      </Install>
      <ManualInstall type="cmd" default="EN">
        <Cmdline id="EN">%PACKAGEPATH%\dpinst_x86.exe</Cmdline>
      </ManualInstall>
      <Uninstall />
      <DetectInstall>
        <False/>
      </DetectInstall>
      <Dependencies>
        <And>
          <False/>
        </And>
      </Dependencies>
      <Files>
        <Installer>
          <File>
            <Name>n2hku07w_x86.exe</Name>
            <CRC>8FA675656A50F06197AE8A29FF36B4DD4CA3F8C564CE3E91A0938259463EB8A0</CRC>
            <Size>1905472</Size>
          </File>
        </Installer>
        <Readme default="EN">
          <File id="EN">
            <Name>n2hku07w.txt</Name>
            <CRC>00C3070610857AF8284D71FF00F77F0633A07B18C059DB84E31E21796C613813</CRC>
            <Size>41736</Size>
          </File>
        </Readme>
      </Files>
    </Package>        
                """

    let testUpdate2 = """<?xml version="1.0" encoding="utf-8"?>
    <?pcdDescriptor version="0230"?>
    <Package name="PMD_N2HKU_64_SKL" id="n2hku07w_x64_skl" version="1.67.16.42" hide="False">
      <Title default="EN">
        <Desc id="EN">Lenovo Power Management Driver - 7/8/8.1/10 [64]</Desc>
      </Title>
      <ReleaseDate>2019-12-18</ReleaseDate>
      <Reboot type="3" />  
      <DetectInstall>
        <And>
          <True/>
    	  <True/>
        </And>
      </DetectInstall>
      <Dependencies>
        <And>
          <True/>
    	  <True/>
        </And>
      </Dependencies>
      <Files>
        <Installer>
          <File>
            <Name>n2hku07w_x64.exe</Name>
            <CRC>D341EA61F07D8C77A50A4165EBE02A9F5BCF4D4E2143475C199D885284C2DBB3</CRC>
            <Size>1905488</Size>
          </File>
        </Installer>
        <Readme default="EN">
          <File id="EN">
            <Name>n2hku07w.txt</Name>
            <CRC>00C3070610857AF8284D71FF00F77F0633A07B18C059DB84E31E21796C613813</CRC>
            <Size>41736</Size>
          </File>
        </Readme>
      </Files>
    </Package>
                """

    let testUpdate3 = """<?xml version="1.0" encoding="utf-8"?>
    <?pcdDescriptor version="0230"?>
    <Package name="HOTKEYPACK4_W10" id="r0yvu32w" version="9.2.0.1" hide="False">
      <Title default="EN">
        <Desc id="EN">ThinkPad Hotkey Features Integration Package Setup - 10 [64]</Desc>
      </Title>  
      <ReleaseDate>2020-09-29</ReleaseDate>  
      <Reboot type="0" />  
      <DetectInstall>
        <And>
    		<True/>
    		<False/>
    	</And>
      </DetectInstall>
      <Dependencies>
        <And>
    		<_Coreq name="PMD_N2HKU_64_SKL">
              <Version>0.1^</Version>
            </_Coreq>
        </And>
      </Dependencies>
      <Files>
        <Installer>
          <File>
            <Name>r0yvu32w.exe</Name>
            <CRC>8157C62CB497156B43AA19E658768D3C49050A204CDF892E01B7A855BB03EF4E</CRC>
            <Size>10542896</Size>
          </File>
        </Installer>
        <Readme default="EN">
          <File id="EN">
            <Name>r0yvu32w.txt</Name>
            <CRC>F9E59D68966BE8C72ED0DF76A0D4FE971F0EE47F352FE26A6DBF42EA592B6EC0</CRC>
            <Size>100550</Size>
          </File>
        </Readme>
      </Files>
    </Package>
                """

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isCoreqMatchTest - Coreq is installed `` () =
        let testResult =
            result{        
                let! update1 = LsupEval.Lsup.loadLsuPackageFromString testUpdate1
                let! update2 = LsupEval.Lsup.loadLsuPackageFromString testUpdate2
                let! update3 = LsupEval.Lsup.loadLsuPackageFromString testUpdate3
                let lsuPackages = [|update1;update2;update3|]
                let! sysInfo = LsupEval.Rules.getCurrentSystemInformation'()
                let isMatch =
                    match update3.Dependencies with                
                    |Some d ->                    
                        let detectionRule = LsupEval.Lsup.lsupXmlToApplicabilityRules logger d
                        let isMatch = LsupEval.Rules.evaluateApplicabilityRule logger sysInfo UpdatesTestData.ExternalFilesFolder (Some lsuPackages) detectionRule 
                        logger.Info(new Msg(fun m -> m.Invoke( (sprintf "Evaluating dependencies: '%s'. Return: %b" update3.Name isMatch))|>ignore))
                        isMatch
                    |None -> false
                return isMatch
            }
        match testResult with        
        |Result.Ok v -> Assert.IsTrue(v,"No errors.")
        |Result.Error ex -> Assert.Fail("Did expect success" + ex.ToString())

    let testUpdate2NotInstalled = """<?xml version="1.0" encoding="utf-8"?>
     <?pcdDescriptor version="0230"?>
     <Package name="PMD_N2HKU_64_SKL" id="n2hku07w_x64_skl" version="1.67.16.42" hide="False">
       <Title default="EN">
         <Desc id="EN">Lenovo Power Management Driver - 7/8/8.1/10 [64]</Desc>
       </Title>
       <ReleaseDate>2019-12-18</ReleaseDate>
       <Reboot type="3" />  
       <DetectInstall>
         <And>
           <True/>
     	  <False/>
         </And>
       </DetectInstall>
       <Dependencies>
         <And>
           <True/>
     	  <True/>
         </And>
       </Dependencies>
       <Files>
         <Installer>
           <File>
             <Name>n2hku07w_x64.exe</Name>
             <CRC>D341EA61F07D8C77A50A4165EBE02A9F5BCF4D4E2143475C199D885284C2DBB3</CRC>
             <Size>1905488</Size>
           </File>
         </Installer>
         <Readme default="EN">
           <File id="EN">
             <Name>n2hku07w.txt</Name>
             <CRC>00C3070610857AF8284D71FF00F77F0633A07B18C059DB84E31E21796C613813</CRC>
             <Size>41736</Size>
           </File>
         </Readme>
       </Files>
     </Package>
                 """
    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``isCoreqMatchTest - Coreq is not installed `` () =
        let testResult =
            result{        
                let! update1 = LsupEval.Lsup.loadLsuPackageFromString testUpdate1
                let! update2 = LsupEval.Lsup.loadLsuPackageFromString testUpdate2NotInstalled
                let! update3 = LsupEval.Lsup.loadLsuPackageFromString testUpdate3
                let lsuPackages = [|update1;update2;update3|]
                let! sysInfo = LsupEval.Rules.getCurrentSystemInformation'()
                let isMatch =
                    match update3.Dependencies with                
                    |Some d ->                    
                        let detectionRule = LsupEval.Lsup.lsupXmlToApplicabilityRules logger d
                        let isMatch = LsupEval.Rules.evaluateApplicabilityRule logger sysInfo UpdatesTestData.ExternalFilesFolder (Some lsuPackages) detectionRule 
                        logger.Info(new Msg(fun m -> m.Invoke( (sprintf "Evaluating dependencies: '%s'. Return: %b" update3.Name isMatch))|>ignore))
                        isMatch
                    |None -> false
                return isMatch
            }
        match testResult with        
        |Result.Ok v -> Assert.IsFalse(v,"No errors.")
        |Result.Error ex -> Assert.Fail("Did expect success" + ex.ToString())


    
    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let detectInstallTest_Driver_IsInstalled_IsTrue () =
        let detectInstall1 = """
        <_Driver>
          <HardwareID><![CDATA[{5425BAE1-FDEE-4761-BB5E-14706E8DC386}]]></HardwareID>
          <Date>2020-04-03</Date>
          <Version>9.2.0.1^</Version>
        </_Driver>
                    """
        let driverInfo =             
            Driver.DriverInfo.Hardware
                               {
                                   HardwareIds=Some [|"{5425BAE1-FDEE-4761-BB5E-14706E8DC386}"|]
                                   CompatibleIds = [||]
                                   Name="Fn and function keys"
                                   Date=(new System.DateTime(2020,04,03))
                                   Version= (Driver.Version "9.2.0.1")
                                   ProviderName="Lenovo"
                               }
        
        let systemInformationTrue = { 
                   BiosVersion = "N1XET1234567"
                   CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
                   Os = "WIN10"
                   OsLang="JP"
                   Drivers= [|driverInfo|]
                   EmbeddedControllerVersion="1.17"
                   PnPIds = [||]
               }
        let detectionRule = LsupEval.Lsup.lsupXmlToApplicabilityRules logger detectInstall1
        let isMatch = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue UpdatesTestData.ExternalFilesFolder None detectionRule
        Assert.IsTrue(isMatch)