﻿//Source: http://www.fssnip.net/7UJ/title/ResultBuilder-Computational-Expression

namespace LsupEval

[<AutoOpen>]
module ResultBuilder =
    open System
    let logger = Logging.getLoggerByName "ResultBuilder"

    let ofOption error = function Some s -> Ok s | None -> Error error
    
    type ResultBuilder() =
        member __.Return(x) = Ok x

        member __.ReturnFrom(m: Result<_, _>) = m

        member __.Bind(m, f) = Result.bind f m
        member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

        member __.Zero() = None

        member __.Combine(m, f) = Result.bind f m

        member __.Delay(f: unit -> _) = f

        member __.Run(f) = f()

        member __.TryWith(m, h) =
            try __.ReturnFrom(m)
            with e -> h e

        member __.TryFinally(m, compensation) =
            try __.ReturnFrom(m)
            finally compensation()

        member __.Using(res:#IDisposable, body) =
            __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

        member __.While(guard, f) =
            if not (guard()) then Ok () else
            do f() |> ignore
            __.While(guard, f)

        member __.For(sequence:seq<_>, body) =
            __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

    let result = new ResultBuilder()

    let sourceException ex = 
        System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).SourceException

    let toException message (innerException: System.Exception option) =
        match innerException with
        |Some iex ->
            (new System.Exception(message, sourceException iex))            
        |None ->
            (new System.Exception(message))

    let toErrorResult message (innerException: System.Exception option) =
        Result.Error (toException message innerException)

    let rec getAccumulatedExceptionMessages (ex: Exception) =
        match ex.InnerException with
        | null -> ex.Message
        | _ -> ex.Message + " " + (getAccumulatedExceptionMessages ex.InnerException)

    let getAllExceptions (results:seq<Result<_,Exception>>) =
            let f = fun (r:Result<_,Exception>) ->
                match r with
                |Error ex -> Some(getAccumulatedExceptionMessages ex)
                |Ok v -> None
            results 
            |> Seq.choose f
    
    let getAllValues (results:seq<Result<_,Exception>>) =
        let f = fun (r:Result<_,Exception>) ->
            match r with
            |Error ex -> None
            |Ok v -> Some(v)
        results 
        |> Seq.choose f

    let toAccumulatedResult (results:seq<Result<_,Exception>>) =
        let resultsArray = results |> Seq.toArray        
        
        let allExceptionMessages = 
                (getAllExceptions resultsArray) 
                |> Seq.toArray
        
        let accumulatedResult =             
            match allExceptionMessages.Length with
            | 0 -> 
                let allValues = getAllValues resultsArray
                Result.Ok allValues
            | _ -> 
                toErrorResult (String.Join<string>(" ", allExceptionMessages)) None
        accumulatedResult

    let resultToOption message result =
        match result with
        |Result.Ok v -> Some v
        |Result.Error (ex:Exception) ->
            logger.Warn(message + " " + ex.Message)
            None