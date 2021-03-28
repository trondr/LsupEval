namespace LsupEval

module ExternalDetection =
    open System
    open System.Diagnostics
    open LsupEval.CommandLineExtensions
    open LsupEval.ProcessOperations
    open LsupEval.Logging

    type ExternalDetection =
        {
            CommandLine:string
            ReturnCodes:int[]
        }

    let isExeFile (fileName:string) =
        fileName.ToLower().EndsWith(".exe")

    type TemporaryWorkingDirectory(workingDirectory) =
        let workingDirectory = workingDirectory
        let previousWorkingDirectory = Environment.CurrentDirectory
        do      
            System.Environment.CurrentDirectory <- workingDirectory
            ()
        interface IDisposable with
            member x.Dispose() =
                System.Environment.CurrentDirectory <- previousWorkingDirectory

    let executeCommandLine commandLine workingDirectory =        
        use temporaryWorkingDirectory = new TemporaryWorkingDirectory(workingDirectory)
        let resolvedCommandLine = LsupEval.File.resolveFilePath commandLine
        match(result{
            let! (cmd,arguments) = toCmdAndArguments resolvedCommandLine
            let tempInstFolder = System.IO.Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.Windows).ToLower(), "TempInst")            
            let processStartData =
                {
                    FileName = LsupEval.File.resolveFilePath cmd
                    Arguments = arguments
                    WorkingDirectory = Some workingDirectory
                    WindowStyle = ProcessWindowStyle.Hidden
                    InputData = None
                    TimeOut = None
                    CreateNoWindow = true
                    UseShellExecute =
                        match (isExeFile cmd) with
                        |true -> false
                        |false-> true
                    EnvironmentVariables = Some (Map["TMP", tempInstFolder])
                }            
            let! processExitData = startConsoleProcess processStartData
            return processExitData
        }) with
        |Result.Ok v -> v
        |Result.Error ex -> raise (toException (sprintf "Failed to execute external detection command '%s'. It is required that the file to be executed is present in the working directory '%s' and if required that the process is started elevated." resolvedCommandLine workingDirectory) (Some ex))

    let returnCodeIsMatch rc returnCodes =
        let isMatch =
            returnCodes 
            |> Array.filter(fun c -> rc = c) //Check if return code is in the list of valid return codes.
            |> Array.tryHead //If valid return code was found Some return code is returned, otherwise None
            |> toBoolean //If Some -> true, if None -> false
        if(logger.IsDebugEnabled) then logger.Debug(sprintf "Comparing return codes: '%A' with driver external detection return code '%A'. Return: %b" returnCodes rc isMatch)
        isMatch