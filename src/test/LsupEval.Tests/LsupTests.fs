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
            let! xDocument = Lsup.loadLsuPackageXDocument @"C:\Temp\LenovoUpdatePackagesXml\nz3gs07w_2_.xml"
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
        |Result.Error ex -> Assert.Fail(ex.Message)