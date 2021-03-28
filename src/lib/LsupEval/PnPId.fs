namespace LsupEval

module PnPId =
    open System.Text.RegularExpressions
    open LsupEval.Logging
    open LsupEval.Driver


    type PnPIdPattern =
        {
            PnPId:string
        }

    let getCurrentPnpIds (driverInfos:seq<DriverInfo>)=
        let pnpIds =
            driverInfos
            |>Seq.map(fun i -> 
                    match i with
                    |Hardware hw -> Some hw
                    |File f -> None
                )            
            |>Seq.choose id
            |>Seq.map(fun i -> i.HardwareIds)
            |>Seq.choose id
            |>Seq.concat
            |>Seq.toArray
        pnpIds

    let isPnPIdMatch (logger:Common.Logging.ILog) (pnPIdPattern:PnPIdPattern) (pnpIds:string[]) =
        let pattern = toRegEx pnPIdPattern.PnPId
        let isMatch =
            pnpIds
            |>Array.filter(fun id -> pattern.IsMatch(id))
            |>Array.tryHead |> toBoolean
        if(logger.IsDebugEnabled) then logger.Debug(sprintf "Comparing pnp ids: '%A' with pnp id pattern '%A'. Return: %b" pnpIds pnPIdPattern isMatch)
        isMatch
        

