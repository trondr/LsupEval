
namespace LsupEval

module Logging=
    
    open Common.Logging

    let getLoggerByName (name:string) = 
        LogManager.GetLogger(name)
