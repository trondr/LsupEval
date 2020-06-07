namespace LsupEval

module CommandLineExtensions=
    open System
    open System.Runtime.InteropServices

    [<DllImport("shell32.dll",SetLastError=true)>]
    extern IntPtr CommandLineToArgvW([<MarshalAs(UnmanagedType.LPWStr)>] string commandLine, int& pNumArgs)

    let commandLineToArgs (commandLine:string) : Result<string[],Exception> =        
        match commandLine with
        |NullOrEmpty -> Result.Ok Array.empty<string>
        |_ ->
            let mutable argc = 0
            let argv = CommandLineToArgvW(commandLine, &argc)
            if(argv = System.IntPtr.Zero) then
                Result.Error (new System.ComponentModel.Win32Exception() :> Exception)
            else
                try
                    let args = 
                        seq{
                            for i in 0 .. (argc-1) do
                                let p = Marshal.ReadIntPtr(argv,i*IntPtr.Size)
                                let arg = Marshal.PtrToStringUni(p)
                                arg
                        }|>Seq.toArray
                    Result.Ok args
                finally
                    Marshal.FreeHGlobal(argv)
    
    let toCmdAndArguments commandLine =
        result{
            let! args = (commandLineToArgs commandLine)
            let argsList = args|> Array.toList
            let! res =
                match argsList with
                |[] -> Result.Error (toException (sprintf "Empty command line %s" commandLine) None)        
                |cmd::argumentsList -> 
                    let arguments = 
                        match argumentsList with
                        |[] -> None
                        |_ -> Some (argumentsList |> String.concat " ")
                    Result.Ok (cmd, arguments)
            return res
        }