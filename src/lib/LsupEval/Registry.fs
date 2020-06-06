namespace LsupEval

module Registry=
    open System
    
    type InvalidRegistryKeyException(version:string, message : string) =
        inherit Exception(
            match String.IsNullOrWhiteSpace(message) with
            |false  -> sprintf "The registry key path '%s' is not valid. %s" version message
            |true -> sprintf "The registry key path '%s' is not valid." version
            )

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
            failwith (sprintf "Invalid registry key path '%s'. Missing hive part." registryKeyPath)

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

    type RegistryKey=
        {            
            Hive:Microsoft.Win32.RegistryKey
            SubKeyPath:string
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

    type ValuePattern =
        |Value of string
        |Version of string
        |Level of string

    let (|RegSZ|RegBinarY|RegDworD|) (regType:string)=
        if(regType = "REG_SZ") then RegSZ
        elif(regType = "REG_BINARY") then RegBinarY
        elif(regType = "REG_DWORD") then RegDworD
        else
            RegSZ

    type RegValueKind =
        |RegSz
        |RegBinary
        |RegDword

    let toRegistryValueKind (regKeyType:string) =
        match regKeyType with
        |RegSZ -> RegSz
        |RegBinarY -> RegSz
        |RegDworD -> RegDword

    type RegistryKeyValuePattern =
        {
            Key:RegistryKey
            ValueKind:RegValueKind
            ValueName:string
            Value:ValuePattern
        }

    let toRegistryKey keyPath =
        {
            Hive = registryKeyPathToHive keyPath
            SubKeyPath = registryKeyPathToSubKeyPath keyPath
        }

    let registryKeyExists (registryKey:RegistryKey) =        
        use key = registryKey.Hive.OpenSubKey(registryKey.SubKeyPath)
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
        
    let registryKeysAreEqual (registryKey1:RegistryKey) (registryKey2:RegistryKey) =
        (registryKey1.Hive.Name = registryKey2.Hive.Name) && (registryKey1.SubKeyPath.ToUpper() = registryKey2.SubKeyPath.ToUpper())


    let isRegistryKeyMatch logger (registryKeyExistsPattern:RegistryKeyExistPattern) (registryKeyStatuses:RegistryKeyStatus[]) =
        let isMatch =
            registryKeyExistsPattern.RegistryKeys
            |>Array.filter(fun r -> 
                    let exists =
                        registryKeyStatuses
                        |>Array.filter(fun rs -> registryKeysAreEqual rs.Key r)
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
        | rkPath -> Result.Ok (toRegistryKey rkPath)
            
    let registryKeyUnsafe registryKeyPath =
        let lc = registryKey registryKeyPath
        match lc with
        |Result.Ok c -> c
        |Result.Error ex -> raise ex

    type RegValueData =
        {
            Value:obj
            ValueKind:RegValueKind
        }

    type RegistryKeyValue =
        {
            Key:RegistryKey
            ValueName:string
            Value:RegValueData option
        }

    let toValueKind (valueKind:Microsoft.Win32.RegistryValueKind) =
        match valueKind with
        |Microsoft.Win32.RegistryValueKind.Binary -> RegValueKind.RegBinary
        |Microsoft.Win32.RegistryValueKind.String -> RegValueKind.RegSz
        |Microsoft.Win32.RegistryValueKind.DWord -> RegValueKind.RegDword   
        |_ -> failwith (sprintf "Value kind not supported: %A" valueKind)

    let toRegistryKeyValue registryKey valueName valueData =
        {
            Key=registryKey
            ValueName=valueName
            Value= valueData
        }

    let getRegistryValue (registryKey:RegistryKey) valueName =
        use key = registryKey.Hive.OpenSubKey(registryKey.SubKeyPath)
        match key with
        |null -> toRegistryKeyValue registryKey valueName None
        |_ -> 
            let value = key.GetValue(valueName)
            match value with
            |null -> toRegistryKeyValue registryKey valueName None
            |_ -> 
                let valueKind = key.GetValueKind(valueName)
                let valueData = Some
                                    {
                                        Value=value
                                        ValueKind=toValueKind valueKind
                                    }
                toRegistryKeyValue registryKey valueName valueData

    let getRegistryKeyValueStatuses (registryKeyValuePattern:RegistryKeyValuePattern) =
        getRegistryValue registryKeyValuePattern.Key registryKeyValuePattern.ValueName



    let isRegistryKeyValueMatch logger (registryKeyValuePattern:RegistryKeyValuePattern) (registryKeyValue:RegistryKeyValue) =
        let isSameRegistryKey = registryKeysAreEqual registryKeyValuePattern.Key registryKeyValue.Key
        let isSameRegistryValue = registryKeyValuePattern.ValueName = registryKeyValue.ValueName        
        let isMatch =
            if(isSameRegistryKey && isSameRegistryValue) then
                match registryKeyValue.Value with
                |None -> false
                |Some vd ->                
                    let rvd = (vd.Value:?>string)
                    match registryKeyValuePattern.Value with
                    |Value v -> v = rvd
                    |Version v -> 
                        let verPattern = LsupEval.Version.versionPatternUnsafe v
                        let ver = LsupEval.Version.versionUnsafe rvd                        
                        LsupEval.Version.isVersionPatternMatch ver verPattern
                    |Level v -> 
                        let regEx = toRegEx v
                        regEx.IsMatch(rvd)
            else
                false
        isMatch        