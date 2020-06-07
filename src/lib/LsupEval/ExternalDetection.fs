namespace LsupEval

module ExternalDetection =
    open System.Diagnostics
    open LsupEval.CommandLineExtensions

    type ExternalDetection =
        {
            CommandLine:string
            ReturnCodes:int[]
        }

    let executeCommandLine commandLine workingDirectory =        
        match(result{
            let! (cmd,arguments) = toCmdAndArguments commandLine
            let processStartInfo =
                match arguments with
                |None -> new System.Diagnostics.ProcessStartInfo(cmd)
                |Some a -> new System.Diagnostics.ProcessStartInfo(cmd,a)
            processStartInfo.WorkingDirectory <- workingDirectory
            processStartInfo.WindowStyle <- ProcessWindowStyle.Hidden
            processStartInfo.CreateNoWindow <- true
            processStartInfo.UseShellExecute <-
                match (cmd.ToLower().EndsWith(".exe")) with
                |true -> false
                |false-> true
            let tmpInstFolder = System.IO.Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.Windows).ToLower(), "TempInst")
            processStartInfo.EnvironmentVariables.["TMP"] <- tmpInstFolder
            let proc = Process.Start(processStartInfo)
            proc.WaitForExit()
            let rc = proc.ExitCode            
            return rc
        }) with
        |Result.Ok v -> v
        |Result.Error ex -> raise ex

    let returnCodeIsMatch rc returnCodes =
        returnCodes 
        |> Array.filter(fun c -> rc = c) //Check if return code is in the list of valid return codes.
        |> Array.tryHead //If valid return code was found Some return code is returned, otherwise None
        |> toBoolean //If Some -> true, if None -> false