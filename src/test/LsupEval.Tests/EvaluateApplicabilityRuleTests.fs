namespace LsupEval.Tests

module EvaluateApplicabilityRuleTests =
    
    open System.IO
    open NUnit.Framework

    open LsupEval;

    //How to get update info for use as test data? Answer:
    //Use debug version of DriverTool.exe to download update info. Command line: DriverTool.exe DownloadLenovUpdatePackageXmls
    //DriverTool.exe will download to: C:\Temp\LenovoUpdatePackagesXml2
    //Move downloaded update info to respective subfolders under LsupEval.Tests\TestData\...

    //let assemblyFile = new System.IO.FileInfo((new System.Uri(System.Reflection.Assembly.GetExecutingAssembly().CodeBase)).LocalPath)
    //let testDataFolderPath = System.IO.Path.Combine(assemblyFile.Directory.Parent.Parent.Parent.FullName,"TestData")
    let testDataFolderPath = @"E:\Dev\github.trondr\LsupEval\src\test\LsupEval.Tests\TestData"
    let externalFilesFolder = System.IO.Path.Combine(testDataFolderPath,"LenovoUpdatePackagesXml\\ExternalFiles")
    let systemInfo = LsupEval.Rules.getCurrentSystemInformation'()
    type internal TestData ={FileName:string}
    let internal testData =        
        [|
            for fileName in UpdatesTestData.P50UpdateFiles do
                yield {FileName = fileName}            
        |]

    [<Test>]
    [<Timeout(3600000)>] //Allow 1 hour run time as it is approx 3000 tests.
    [<Category(TestCategory.ManualTests)>]
    [<TestCaseSource("testData")>]
    let ``load Lsu Package evaluate depencies `` (testDataObject:obj) =
        let testResult = 
            result{
                let testData = (testDataObject:?>TestData)
                let lsuPackageFilePath = System.IO.Path.Combine(testDataFolderPath,"LenovoUpdatePackagesXml\\Updates",testData.FileName)
                let! lsuPackage = LsupEval.Lsup.loadLsuPackageFromFile(lsuPackageFilePath)
                let! sysInfo = systemInfo
                let isMatch =
                    match lsuPackage.Dependencies with                
                    |Some d ->                    
                        let detectionRule = LsupEval.Lsup.lsupXmlToApplicabilityRules logger d
                        let isMatch = LsupEval.Rules.evaluateApplicabilityRule logger sysInfo externalFilesFolder detectionRule
                        isMatch
                    |None -> false
                return isMatch
            }
        match testResult with        
        |Result.Ok v -> Assert.IsTrue(v,"No errors.")
        |Result.Error ex -> Assert.Fail("Did expect success" + ex.ToString())


    [<Test>]
    [<Timeout(3600000)>] //Allow 1 hour run time as it is approx 3000 tests.
    [<Category(TestCategory.ManualTests)>]
    [<TestCaseSource("testData")>]
    let ``load Lsu Package evaluate detect install `` (testDataObject:obj) =
        let testResult = 
            result{
                let testData = (testDataObject:?>TestData)
                let lsuPackageFilePath = System.IO.Path.Combine(testDataFolderPath,"LenovoUpdatePackagesXml\\Updates",testData.FileName)
                let! lsuPackage = LsupEval.Lsup.loadLsuPackageFromFile(lsuPackageFilePath)
                let! sysInfo = systemInfo
                let isMatch =
                    match lsuPackage.DetectInstall with                
                    |Some d ->                    
                        let detectionRule = LsupEval.Lsup.lsupXmlToApplicabilityRules logger d
                        let isMatch = LsupEval.Rules.evaluateApplicabilityRule logger sysInfo externalFilesFolder detectionRule
                        isMatch
                    |None -> false
                return isMatch
            }
        match testResult with        
        |Result.Ok v -> Assert.IsTrue(true,"No errors.")
        |Result.Error ex -> Assert.Fail("Did expect success" + ex.ToString())
