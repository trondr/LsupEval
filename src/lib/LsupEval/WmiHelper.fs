namespace LsupEval

module WmiHelper =
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
