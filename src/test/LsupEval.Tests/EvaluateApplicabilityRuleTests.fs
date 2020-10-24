namespace LsupEval.Tests

module EvaluateApplicabilityRuleTests =
    
    open System.IO
    open NUnit.Framework

    open LsupEval;

    let assemblyFile = new System.IO.FileInfo((new System.Uri(System.Reflection.Assembly.GetExecutingAssembly().CodeBase)).LocalPath)
    let testDataFolderPath = System.IO.Path.Combine(assemblyFile.Directory.Parent.Parent.Parent.FullName,"TestData")
    type internal TestData ={FileName:string}
    let internal testData =
        let updatesDirectory = System.IO.Path.Combine(testDataFolderPath,"LenovoUpdatePackagesXml\\Updates")
        let files = System.IO.Directory.GetFiles(updatesDirectory)
        [|
            for file in files do            
                let fileinfo = new FileInfo(file)                
                yield 
                    {FileName = fileinfo.Name}
        |]
    let systemInfo = LsupEval.Rules.getCurrentSystemInformation()
    let externalFilesFolder = System.IO.Path.Combine(testDataFolderPath,"LenovoUpdatePackagesXml\\ExternalFiles")

    [<Test>]
    [<Timeout(60000000)>]
    [<Category(TestCategory.ManualTests)>]
    [<TestCaseSource("testData")>]
    let ``load Lsu Package evaluate depencies and detect install `` (testDataObject:obj) =
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
        |Result.Ok v -> Assert.IsTrue(v,"Did not expect success")
        |Result.Error ex -> Assert.Fail("Did expect success" + ex.ToString())
