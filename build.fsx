#r "paket:
nuget FSharp.Core 4.7.0.0
nuget NUnit.ConsoleRunner
nuget Fake.IO.FileSystem
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Testing.Nunit
nuget Fake.Testing.Common
nuget Fake.DotNet.NuGet
nuget Fake.IO.Zip
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.IO
open Fake.IO.Globbing.Operators //enables !! and globbing
open Fake.Core
open Fake.DotNet.Testing
open Fake.DotNet

//Properties
let buildFolder = System.IO.Path.GetFullPath("./build/")
let buildLibFolder = buildFolder + "lib"
let buildTestFolder = buildFolder + "test"
let artifactFolder = System.IO.Path.GetFullPath("./artifact/")

let globalPackagesFolder =     
    System.Environment.ExpandEnvironmentVariables("%userprofile%\.nuget\packages")

let getVersion file = 
    System.Reflection.AssemblyName.GetAssemblyName(file).Version.ToString()

//Targets
Target.create "Clean" (fun _ ->
    Trace.trace "Clean folders..."
    Shell.cleanDirs [ buildFolder; artifactFolder ]
)

Target.create "RestorePackages" (fun _ ->
     "./lsupeval.sln"
     |> Fake.DotNet.NuGet.Restore.RestoreMSSolutionPackages (fun p ->
         { p with             
             Retries = 4 })
   )

Target.create "BuildLib" (fun _ -> 
    Trace.trace "Building lib..."    
    !! "src/lib/**/*.fsproj"
        |> Fake.DotNet.MSBuild.runRelease id buildLibFolder "Build"
        |> Trace.logItems "BuildApp-Output: "
)

Target.create "BuildTest" (fun _ -> 
    Trace.trace "Building test..."
    !! "src/test/**/*.fsproj"
        |> Fake.DotNet.MSBuild.runRelease id buildTestFolder "Build"
        |> Trace.logItems "BuildTest-Output: "
)

let nunitConsoleRunner() =
    let consoleRunner = 
        let search = 
            !! (globalPackagesFolder + "/**/nunit3-console.exe")
        if (Seq.isEmpty search) then 
            (failwith "Could not locate nunit3-console.exe in ./packages folder.")
        else 
            (search |> Seq.head)        
    printfn "Console runner:  %s" consoleRunner
    consoleRunner

Target.create "Test" (fun _ -> 
    Trace.trace "Testing app..."    
    !! ("build/test/**/*.Tests.dll")    
    |> NUnit3.run (fun p ->
        {p with ToolPath = nunitConsoleRunner();Where = "cat==UnitTests";TraceLevel=NUnit3.NUnit3TraceLevel.Verbose})
)

Target.create "Publish" (fun _ ->
    Trace.trace "Publishing app..."    
    let files = 
        !! ("build/lib/**/*.nupkg")
        ++ ("build/lib/**/*.snupkg")
    files |> Shell.copy artifactFolder    
    files|> Shell.copy "E:\NugetRepository"
)

Target.create "Default" (fun _ ->
    Trace.trace "Hello world from FAKE"
)

//Dependencies
open Fake.Core.TargetOperators

"Clean" 
    ==> "RestorePackages"
    ==> "BuildLib"
    ==> "BuildTest"
    //==> "Test"
    ==> "Publish"
    ==> "Default"

//Start build
Target.runOrDefault "Default"