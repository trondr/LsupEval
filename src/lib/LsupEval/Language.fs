namespace LsupEval

module Language=
    open System

    type InvalidLanguageCodeException(version:string, message : string) =
        inherit Exception(
            match String.IsNullOrWhiteSpace(message) with
            |false  -> sprintf "The language code '%s' is not valid. %s" version message
            |true -> sprintf "The language code '%s' is not valid." version
            )

    let private languageCodeRegex = new System.Text.RegularExpressions.Regex("((E|e)(N|n))|((F|f)(R|r))|((I|i)(T|t))|((D|d)(E|e))|((E|e)(S|s))|((N|n)(O|o))|((S|s)(V|v))|((F|f)(I|i))|((D|d)(A|a))|((N|n)(L|l))|((P|p)(T|t)(G|g))|((P|p)(T|t)(B|b))|((A|a)(R|r))|((C|c)(S|s))|((E|e)(L|l))|((H|h)(E|e))|((H|h)(U|u))|((P|p)(L|l))|((R|r)(U|u))|((T|t)(R|r))|((J|j)(P|p))|((K|k)(O|o))|((C|c)(H|h)(S|s))|((C|c)(H|h)(T|t))|(\*)",System.Text.RegularExpressions.RegexOptions.Compiled)

    let isNotLanguageCode languageCodeString =
        not (languageCodeRegex.IsMatch(languageCodeString))

    let private (|IsNotLanguageCode|_|) =
        isNotLanguageCode
        >> ifTrueThen IsNotLanguageCode

    type LanguagePattern =
        {
            LanguageCode : string
        }

    let language languageCode =
        match languageCode with
        |null -> Result.Error (new InvalidLanguageCodeException("","Language code cannot be null.") :> Exception)
        |NullOrEmpty -> Result.Error (new InvalidLanguageCodeException("","Language code cannot be empty.") :> Exception)
        |IsNotLanguageCode -> Result.Error (new InvalidLanguageCodeException(languageCode,"Language code is not in the valid range.") :> Exception)
        | lc -> Result.Ok
                    {
                        LanguageCode = lc
                    }
            
    let languageUnsafe languageCode =
        let lc = language languageCode
        match lc with
        |Result.Ok c -> c
        |Result.Error ex -> raise ex