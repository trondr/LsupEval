namespace LsupEval.Tests

module CommandLineExtensionTests =
    open LsupEval.ResultBuilder
    open NUnit.Framework

    let commandLineToArgsTest (commandLine:string, expected:string[]) =
        match(result{
            let! actual = LsupEval.CommandLineExtensions.commandLineToArgs commandLine    
            return actual
        })with
        |Result.Ok a ->                   
            Assert.AreEqual(expected,a)
        |Result.Error ex -> Assert.Fail(ex.Message)

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    let emptycommandLineToArgsTests () = 
        commandLineToArgsTest("",[||])

    [<Test>]
    [<Category(TestCategory.UnitTests)>]
    [<TestCase("%PACKAGEPATH%\getw10ver7.exe",[|"%PACKAGEPATH%\getw10ver7.exe"|])>]
    [<TestCase("%PACKAGEPATH%\usi002_tesla_version.exe /FW Ver2.22.000",[|"%PACKAGEPATH%\usi002_tesla_version.exe";"/FW";"Ver2.22.000"|])>]
    [<TestCase("%PACKAGEPATH%\wldcgt03_check_update.exe /check /verysilent /SUPPRESSMSGBOXES",[|"%PACKAGEPATH%\wldcgt03_check_update.exe";"/check";"/verysilent";"/SUPPRESSMSGBOXES"|])>]    
    let commandLineToArgsTests2 (commandLine:string,expectedArray:string[]) =             
        commandLineToArgsTest(commandLine,expectedArray)


