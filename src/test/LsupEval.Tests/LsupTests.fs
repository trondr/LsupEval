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