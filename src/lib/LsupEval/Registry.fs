namespace LsupEval

module Registry=
    open System
    
    type InvalidRegistryKeyException(version:string, message : string) =
        inherit Exception(
            match String.IsNullOrWhiteSpace(message) with
            |false  -> sprintf "The registry key path '%s' is not valid. %s" version message
            |true -> sprintf "The registry key path '%s' is not valid." version
            )

    type RegistryKey=
        {
            KeyPath:string
        }
    
    type RegistryKeyExistPattern =
        {
            RegistryKeys:RegistryKey[]
        }

    type RegistryKeyStatus=
        {
            Key: RegistryKey
            Exists: bool        
        }

    let (|HKLM|HKCU|HKCR|HKU|HKCC|) (registryKeyPath:string) =
        if (registryKeyPath.StartsWith("HKEY_LOCAL_MACHINE") || registryKeyPath.StartsWith("HKLM")) then 
            HKLM
        elif (registryKeyPath.StartsWith("HKEY_CURRENT_USER") || registryKeyPath.StartsWith("HKCU")) then
            HKCU
        elif (registryKeyPath.StartsWith("HKEY_CLASSES_ROOT") || registryKeyPath.StartsWith("HKCR")) then
            HKCR
        elif (registryKeyPath.StartsWith("HKEY_USERS") || registryKeyPath.StartsWith("HKU")) then
            HKU
        elif (registryKeyPath.StartsWith("HKEY_CURRENT_CONFIG") || registryKeyPath.StartsWith("HKCC")) then
            HKCC
        else
            HKCR

    let registryKeyPathToSubKeyPath (registryKeyPath:string) =
        match registryKeyPath with
        |HKLM -> System.Text.RegularExpressions.Regex.Replace(registryKeyPath,"(HKEY_LOCAL_MACHINE|HKLM)\\\\","")            
        |HKCU -> System.Text.RegularExpressions.Regex.Replace(registryKeyPath,"(HKEY_CURRENT_USER|HKCU)\\\\","")
        |HKCR -> System.Text.RegularExpressions.Regex.Replace(registryKeyPath,"(HKEY_CLASSES_ROOT|HKCR)\\\\","")
        |HKU -> System.Text.RegularExpressions.Regex.Replace(registryKeyPath,"(HKEY_USERS|HKU)\\\\","")
        |HKCC -> System.Text.RegularExpressions.Regex.Replace(registryKeyPath,"(HKEY_CURRENT_CONFIG|HKCC)\\\\","")


    let registryKeyPathToHive (registryKeyPath:string) =
       match registryKeyPath with
       |HKLM -> Microsoft.Win32.Registry.LocalMachine
       |HKCU -> Microsoft.Win32.Registry.CurrentUser
       |HKCR -> Microsoft.Win32.Registry.ClassesRoot
       |HKU -> Microsoft.Win32.Registry.Users
       |HKCC -> Microsoft.Win32.Registry.CurrentConfig       

    let registryKeyExists (registryKey:RegistryKey) =
        let hive = registryKeyPathToHive registryKey.KeyPath
        let subKeyPath = registryKeyPathToSubKeyPath registryKey.KeyPath
        use key = hive.OpenSubKey(subKeyPath)
        match key with
        |null -> false
        |_ -> true

    let getRegistryKeyStatusesFromRegistryKeyExistPattern (registryKeyExistsPattern:RegistryKeyExistPattern) =
        registryKeyExistsPattern.RegistryKeys
        |>Seq.map(fun r -> 
                
                let exists = registryKeyExists r
                {
                    Key=r
                    Exists=exists
                }            
            )
        |>Seq.toArray
        
    let isRegistryKeyMatch logger (registryKeyExistsPattern:RegistryKeyExistPattern) (registryKeyStatuses:RegistryKeyStatus[]) =
        let isMatch =
            registryKeyExistsPattern.RegistryKeys
            |>Array.filter(fun r -> 
                    let exists =
                        registryKeyStatuses
                        |>Array.filter(fun rs -> rs.Key.KeyPath.ToUpper() = r.KeyPath.ToUpper())
                        |>Array.filter(fun rs -> rs.Exists)
                        |>Array.tryHead
                        |>toBoolean
                    exists
                )
            |>Array.tryHead
            |>toBoolean
        isMatch


    let registryKey (registryKeyPath:string) =
        match registryKeyPath with
        |null -> Result.Error (new InvalidRegistryKeyException("","Registry key path cannot be null.") :> Exception)
        |NullOrEmpty -> Result.Error (new InvalidRegistryKeyException("","Registry key path code cannot be empty.") :> Exception)        
        | rkPath -> Result.Ok
                        {
                            KeyPath = rkPath
                        }
            
    let registryKeyUnsafe registryKeyPath =
        let lc = registryKey registryKeyPath
        match lc with
        |Result.Ok c -> c
        |Result.Error ex -> raise ex