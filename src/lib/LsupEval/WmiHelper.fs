namespace LsupEval

module WmiHelper =
    open System
    open System.Management

    let getWmiPropertyValue className propertyName =
        try
            use searcher = new ManagementObjectSearcher(sprintf "SELECT %s FROM %s" propertyName className)
            use list = searcher.Get()
            let propertyValue = 
                list
                |>Seq.cast
                |>Seq.map(fun (x:ManagementObject) -> x.[propertyName])
                |>Seq.head
            Result.Ok propertyValue
        with
        |ex -> 
            Result.Error (toException (sprintf "Failed to get property value '%s' from class '%s'." propertyName className) (Some ex))

    let objectToString (value:obj) =
        try
            Result.Ok (value :?> string)
        with
        |ex -> Result.Error ex

    let objectToInt32 (value:obj) =
        try
            Result.Ok (value :?> Int32)
        with
        |ex -> Result.Error ex


    let getWmiObjects className propertyNames =
        try
            let properties = propertyNames |> String.concat ","
            use searcher = new ManagementObjectSearcher(sprintf "SELECT %s FROM %s" properties className)
            use list = searcher.Get()
            let wmiObjects = 
                list
                |>Seq.cast
                |>Seq.map(fun (x:ManagementObject) ->
                            seq{
                                yield! propertyNames |> Seq.map(fun pn ->(pn, x.[pn]))
                            }
                            |>Map.ofSeq
                    )
            Result.Ok wmiObjects
        with
        |ex -> Result.Error (toException (sprintf "Failed to get wmi object property values '%A' from class '%s'." propertyNames className) (Some ex))
