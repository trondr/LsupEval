
namespace LsupEval

module internal Logging=
    
    open Common.Logging

    let getLoggerByName (name:string) = 
        LogManager.GetLogger(name)

    type Msg = System.Action<Common.Logging.FormatMessageHandler>
    let msg message =
        new Msg(fun m -> m.Invoke(message)|>ignore)
