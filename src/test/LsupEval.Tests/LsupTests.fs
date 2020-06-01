namespace LsupEval.Tests

module LsupTest =
    
    open NUnit.Framework
    open LsupEval;

    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``loadLsuPackageXDocument xml file exists return OK`` () =
        let actual = Lsup.loadLsuPackageXDocument @"C:\Temp\LenovoUpdatePackagesXml\nz3gs07w_2_.xml"
        match actual with
        |Result.Ok -> Assert.IsTrue(true)
        |Result.Error ex -> Assert.Fail(ex.Message)


    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``loadLsuPackageXDocument xml file does not exists return Error`` () =
        let actual = Lsup.loadLsuPackageXDocument @"C:\Temp\LenovoUpdatePackagesXml\nz3gs07w_2_Does_Not_Exist.xml"
        match actual with
        |Result.Ok -> Assert.IsTrue(false, "Expected error result")
        |Result.Error ex -> Assert.AreEqual("Failed to load 'C:\\Temp\\LenovoUpdatePackagesXml\\nz3gs07w_2_Does_Not_Exist.xml'. Could not find file 'C:\\Temp\\LenovoUpdatePackagesXml\\nz3gs07w_2_Does_Not_Exist.xml'.",ex.Message)


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
            Assert.AreEqual(Lsup.LsuRebootType.RebootRequired,p.RebootType,"Package.RebootType")
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
        |Result.Ok -> Assert.IsTrue(true)
        |Result.Error ex -> Assert.Fail(ex.Message)  
        
    [<Test>]
    [<Category(TestCategory.ManualTests)>]
    let ``getCurrentCpuAddressWidth a machine specific test on Lenovo ThinkPad P50 on 2020-05-24`` () =
        match(result{
            let! actual = Cpu.getCurrentCpuAddressWidth()
            Assert.AreEqual(Cpu.CpuAddressWidth.Bit64,actual,"Cpu address width")
            return actual
        })with
        |Result.Ok -> Assert.IsTrue(true)
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
                Drivers= [||]
            }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules
        printf "%A" applicabilityRule
        let actual = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationFalse applicabilityRule 
        let expecedFalse = false
        Assert.AreEqual(expecedFalse,actual,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationFalse)


    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let ``lsupXmlToApplicabilityRules Has the Hardware but is Non-Matching on driver version`` () =
        let systemInformationTrue = { 
            BiosVersion = "N1XET1234567"
            CpuAddressWidth = Cpu.CpuAddressWidth.Bit64
            Os = "WIN10"
            Drivers= 
                [|
                    Driver.DriverInfo.Hardware
                        {
                            HardwareIds=[|"PCI\VEN_1022&DEV_1537"|]
                            CompatibleIds = [||]
                            Name="Test Name"
                            Date=(new System.DateTime(2019,05,21))
                            Version= (Driver.Version "4.8.0.0")
                            ProviderName="Lenovo"
                        }
                |]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue applicabilityRule 
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
            Drivers= 
                [|
                    Driver.DriverInfo.Hardware
                        {
                            HardwareIds=[|"PCI\VEN_1022&DEV_1537"|]
                            CompatibleIds = [||]
                            Name="Test Name"
                            Date=(new System.DateTime(2019,05,21))
                            Version= (Driver.Version "4.10.0.0")
                            ProviderName="Lenovo"
                        }
                |]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue applicabilityRule 
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
            Drivers= 
                [|
                    Driver.DriverInfo.Hardware
                        {
                            HardwareIds=[|"PCI\VEN_1022&DEV_1537"|]
                            CompatibleIds = [||]
                            Name="Test Name"
                            Date=(new System.DateTime(2019,04,21))
                            Version= (Driver.Version "4.10.0.0")
                            ProviderName="Lenovo"
                        }
                |]
        }
        let applicabilityRule = Lsup.lsupXmlToApplicabilityRules logger applicabiliyRules
        let actual2 = LsupEval.Rules.evaluateApplicabilityRule logger systemInformationTrue applicabilityRule 
        let expectedTrue = false
        Assert.AreEqual(expectedTrue,actual2,sprintf "Evaulation of applicability rule '%A' with  system information '%A'" applicabilityRule systemInformationTrue)
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