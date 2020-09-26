namespace LsupEval

module ProcessOperations =
    open System
    open System.Diagnostics
    open System.Text
    open System.Collections.Generic
    
    type ProcessStartData = 
        {
            FileName:string;
            Arguments:string option;
            WorkingDirectory:string option;
            InputData:string option;
            TimeOut:int option;
            WindowStyle:ProcessWindowStyle;
            CreateNoWindow:bool;
            UseShellExecute:bool;
            EnvironmentVariables:Map<string,string> option
        }

    type ProcessExitData = {ExitCode:int; StdOutput:string; StdError:string}

    let toCommandLine fileName arguments =
        match arguments with
        |Some args ->
            sprintf "\"%s\" %s" fileName args
        |None ->
            sprintf "\"%s\"" fileName

    let startConsoleProcessUnsafe processStartData =
        let startInfo = new ProcessStartInfo()
        startInfo.CreateNoWindow <- true
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.FileName <- processStartData.FileName
        match processStartData.Arguments with | Some args -> startInfo.Arguments <- args |None -> ()
        match processStartData.WorkingDirectory with | Some wd -> startInfo.WorkingDirectory <- wd |None -> ()
        match processStartData.EnvironmentVariables with
        |Some d ->
            d
            |>Seq.map(fun kv -> 
                    startInfo.EnvironmentVariables.[kv.Key] <- d.[kv.Key]
                )
            |>Seq.toArray |> ignore
        |None -> ()

        use consoleProcess = new Process()
        consoleProcess.StartInfo <- startInfo
        match processStartData.InputData with 
        | Some inputData -> 
            consoleProcess.StandardInput.Write(inputData) 
            consoleProcess.StandardInput.Close()
        |None -> ()

        let stdOutBuilder = new StringBuilder()
        let stdErrorBuilder = new StringBuilder()

        let outputDataReceivedHandler =
           new DataReceivedEventHandler(fun _ e ->                                             
                                            stdOutBuilder.AppendLine(e.Data) |> ignore
                                       )
        let errorDataReceivedHandler =           
           new DataReceivedEventHandler(fun _ e ->                                             
                                            stdErrorBuilder.AppendLine(e.Data) |> ignore                                     
                                            )

        consoleProcess.OutputDataReceived.AddHandler(outputDataReceivedHandler)
        consoleProcess.ErrorDataReceived.AddHandler(errorDataReceivedHandler)

        consoleProcess.EnableRaisingEvents <- true
        consoleProcess.Start() |> ignore
        consoleProcess.BeginOutputReadLine()
        consoleProcess.BeginErrorReadLine()
        let isWaitSuccesful = 
            match processStartData.TimeOut with
            |Some t ->
                consoleProcess.WaitForExit(t)
            |None ->
                consoleProcess.WaitForExit()
                true
        
        match isWaitSuccesful with
        |true -> ignore
        |false ->            
            consoleProcess.Close()
            let commandLine = toCommandLine processStartData.FileName processStartData.Arguments
            raise (new Exception(sprintf "Process execution timed out: %s" commandLine))
        |>ignore
            
        consoleProcess.CancelErrorRead()
        consoleProcess.CancelOutputRead()
        consoleProcess.ErrorDataReceived.RemoveHandler(errorDataReceivedHandler)
        consoleProcess.OutputDataReceived.RemoveHandler(outputDataReceivedHandler)
        let processExitData =
            {                
                ExitCode = consoleProcess.ExitCode
                StdOutput = stdOutBuilder.ToString().Trim()
                StdError = stdErrorBuilder.ToString().Trim()
            }
        processExitData

    let startConsoleProcess processStartData =
        try
            let processExitData = startConsoleProcessUnsafe processStartData
            Result.Ok processExitData
        with
        |ex -> Result.Error ex