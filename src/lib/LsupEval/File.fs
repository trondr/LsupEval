namespace LsupEval

module File =
    open LsupEval.Logging

    type FileExistsElement =
        {
            FilePath:string
        }

    type FileStatus =
        {
            FilePath:string
            Exists:bool
        }

    let windowsFolder =
        System.Environment.GetFolderPath(System.Environment.SpecialFolder.Windows)
    let systemFolder =
        System.Environment.GetFolderPath(System.Environment.SpecialFolder.System)
    let tempFolder =
        System.IO.Path.GetTempPath()
    let packagePath () =
        System.Environment.CurrentDirectory
    let regex1 = new System.Text.RegularExpressions.Regex("%WINDOWS%", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    let regex2 = new System.Text.RegularExpressions.Regex("%SYSTEM%", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    let regex3 = new System.Text.RegularExpressions.Regex("%TEMP%", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    let regex4 = new System.Text.RegularExpressions.Regex("%PACKAGEPATH%", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    
    ///<summary>
    /// Replace Lenovo environment variables in file path. Example: Replace "%WINDOWS%" with "C:\Windows"
    ///</summary>
    let resolveFilePath (filePath:string) =                
        let regExArray = [|(regex1,windowsFolder);(regex2,systemFolder);(regex3,tempFolder);(regex4,packagePath())|]
        let replace (text:string) (regex:System.Text.RegularExpressions.Regex,replacementText:string) =
            regex.Replace(text,replacementText)                
        let resolvedFilePath = Array.fold (fun fp (r,t)-> replace fp (r,t)) filePath regExArray
        resolvedFilePath

    let getFilesFromFileExistPattern (fileExistsElement:FileExistsElement) =
        let filePath = resolveFilePath fileExistsElement.FilePath
        {
            FilePath= filePath
            Exists=System.IO.File.Exists(filePath)
        }

    let isFileExistsMatch (logger:Common.Logging.ILog) (fileExistsElement:FileExistsElement) (file:FileStatus) =
        let filePath = resolveFilePath fileExistsElement.FilePath
        let isMatch = (filePath.ToUpper() = file.FilePath.ToUpper()) && file.Exists
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing file exist status: '%A' with file exists pattern '%A'. Return: %b" file fileExistsElement isMatch))|>ignore))
        isMatch
        
        


