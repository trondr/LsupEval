namespace LsupEval

module Version =
    open System
    open System.Linq
    open LsupEval.Logging
    let logger = getLoggerByName "LsupEval.Version"

    type InvalidVersionException(version:string, message : string) =
        inherit Exception(
            match String.IsNullOrWhiteSpace(message) with
            |false  -> sprintf "The version '%s' is not valid. %s" version message
            |true -> sprintf "The version '%s' is not valid." version
            )

    type VersionBlock =
        |Numeric of int16
        |AlphaNumeric of string

    type LsupVersion = private LsupVersion of VersionBlock[]

    let private ifTrueThen success =
        function
        |true -> Some success
        |false -> None

    let private (|NullOrEmpty|_|) =
        String.IsNullOrWhiteSpace 
        >> ifTrueThen NullOrEmpty

    let isNumericChar (c:char) =        
        if (c >= '0') then
            (c <= '9')
        else
            false

    /// <summary>
    /// Check each character in string if it is numeric. If all characters are numberic return true.
    /// </summary>
    let isNumericString (s:string) =
        let isNumericArray = 
            s
            |>Seq.toArray
            |>Array.map (fun c -> isNumericChar c)        
        match isNumericArray with
        |[||] -> false
        |_ -> isNumericArray.All(fun b -> b)

    let toVersionBlock (versionBlock:string) =
        if(isNumericString versionBlock) then
            VersionBlock.Numeric (Convert.ToInt16(versionBlock))
        else
            VersionBlock.AlphaNumeric versionBlock

    let private toVersionBlocks (version:string) =
        let stringArray = version.Replace(" ", "").Split([|'.'|])
        let emptyBlock = stringArray|>Array.filter(fun sb-> String.IsNullOrEmpty(sb)) |> Array.tryHead
        match emptyBlock with
        |Some _ -> Result.Error (new InvalidVersionException(version,"Version cannot contain empty blocks.") :> Exception)
        |None ->
            Result.Ok (stringArray |>Array.map toVersionBlock)
        
    let version version =
        match version with
        |null -> Result.Error (new InvalidVersionException("","Version cannot be null.") :> Exception)
        |NullOrEmpty -> Result.Error (new InvalidVersionException("","Version cannot be empty.") :> Exception)
        |v -> 
            result{
                let! versionBlocks = toVersionBlocks v            
                return (LsupVersion versionBlocks)
            }
        
    let min size1 size2 =
        if(size1 < size2) then
            size1
        else
            size2
    
    let compareInt n1 n2 =
        if(n1 = n2) then
            0
        else if (n1 < n2) then
            -1
        else
            1

    let compareVersionBlock versionBlock1 versionBlock2 =
        match versionBlock1 with
        |VersionBlock.Numeric n1 ->
            match versionBlock2 with
            |VersionBlock.Numeric n2 -> 
                compareInt n1 n2
            |VersionBlock.AlphaNumeric s2 ->
                String.Compare(n1.ToString(),s2)
        |VersionBlock.AlphaNumeric s1 -> 
            match versionBlock2 with
            |VersionBlock.Numeric n2 -> 
                String.Compare(s1,n2.ToString())
            |VersionBlock.AlphaNumeric s2 ->
                String.Compare(s1,s2)

    let compare (LsupVersion versionBlocks1) (LsupVersion versionBlocks2) =
        let numberOfBlocksToCompare = min versionBlocks1.Length versionBlocks2.Length
        let versionBlocks1Sliced = versionBlocks1.[..numberOfBlocksToCompare-1]
        let versionBlocks2Sliced = versionBlocks2.[..numberOfBlocksToCompare-1]
        let zippedVersionBlocks = Array.zip versionBlocks1Sliced versionBlocks2Sliced //Allign pairwize version blocks to be compared
        let firstNonEqualVersionBlocks =
            zippedVersionBlocks 
            |> Array.filter(fun (v1,v2) -> (compareVersionBlock v1 v2) <> 0 ) //Remove version blocks that are eqal
            |> Array.tryHead //Get the first version block that is different or None if all blocks are equal
        match firstNonEqualVersionBlocks with
        |Some (v1,v2) -> compareVersionBlock v1 v2
        |None -> 0
