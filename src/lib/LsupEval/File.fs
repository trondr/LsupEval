namespace LsupEval

module File =
    open LsupEval.Logging
    open System.Diagnostics

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
        System.Environment.ExpandEnvironmentVariables(resolvedFilePath)

    let fileExists filePath =
        System.IO.File.Exists(filePath)

    let getFilesFromFileExistPattern (fileExistsElement:FileExistsElement) =
        let filePath = resolveFilePath fileExistsElement.FilePath
        {
            FilePath= filePath
            Exists=fileExists filePath
        }

    let isFilePathMatch (filePath1:string) (filePath2:string) =
        filePath1.ToUpper() = filePath2.ToUpper()

    let isFileExistsMatch (logger:Common.Logging.ILog) (fileExistsElement:FileExistsElement) (file:FileStatus) =
        let filePath = resolveFilePath fileExistsElement.FilePath
        let isMatch = 
            (isFilePathMatch filePath file.FilePath) && file.Exists            
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing file exist status: '%A' with file exists pattern '%A'. Return: %b" file fileExistsElement isMatch))|>ignore))
        isMatch

    let getFileVersion filePath =
        let fileVersion = FileVersionInfo.GetVersionInfo(filePath);
        (sprintf "%d.%d.%d.%d" fileVersion.FileMajorPart fileVersion.FileMinorPart fileVersion.FileBuildPart fileVersion.FilePrivatePart)

    type FileVersionPattern=
        {
            FilePath:string
            VersionPattern:LsupEval.Version.LsupVersionPattern        
        }

    type FileVersion =
        {
            FileStatus:FileStatus
            Version:LsupEval.Version.LsupVersion
        }

    let getLsupVersion filePath =
        let exists = fileExists filePath
        match exists with
        | true ->
            let fileVersion = getFileVersion filePath
            result{
                let! version = LsupEval.Version.version fileVersion
                return version
            }
        | false -> (LsupEval.Version.version "0.0.0.0")

    let getFileVersionFromFileVersionPattern (fileversionPattern:FileVersionPattern) =
        match(result{
            let filePath = resolveFilePath fileversionPattern.FilePath
            let fileExists = fileExists filePath            
            let! version = getLsupVersion filePath
            return
                {            
                    FileStatus=
                        {
                            FilePath=filePath
                            Exists=fileExists
                        }
                    Version=version
                }
        })with
        |Result.Ok ver -> ver
        |Result.Error ex -> raise ex
        
    let isFileVersionMatch (logger:Common.Logging.ILog) (fileversionPattern:FileVersionPattern) (fileVersion:FileVersion) =
        let filePathPattern = resolveFilePath fileversionPattern.FilePath
        let fileExists = lazy (fileVersion.FileStatus.Exists)
        let isSameFile = lazy (isFilePathMatch filePathPattern fileVersion.FileStatus.FilePath)        
        let isVersionMatch = lazy (LsupEval.Version.isVersionPatternMatch fileVersion.Version fileversionPattern.VersionPattern)
        let isMatch = fileExists.Force() && isSameFile.Force() && isVersionMatch.Force()
        logger.Debug(new Msg(fun m -> m.Invoke( (sprintf "Comparing file version: '%A' with file version pattern '%A'. Return: %b" fileVersion fileversionPattern isMatch))|>ignore))
        isMatch

        
        
        
        


