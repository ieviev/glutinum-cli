module Naming

open System

[<Literal>]
let MODULE_PLACEHOLDER = "REPLACE_ME_WITH_MODULE_NAME"

let private startWithDigit (name: string) : bool =
    name.Length > 0 && Char.IsDigit name.[0]

let invalidchars = "|=-~&~$()#<> "

let private escapeName (name: string) : string =
    if
        not (name.StartsWith("``"))
        && (invalidchars |> String.exists (fun v -> name.IndexOf(v) <> -1)
            || startWithDigit name
            || Keywords.fsharp.Contains name)
    then
        $"``%s{name}``"
    else
        name

let removeSurroundingQuotes (text: string) : string =
    if String.IsNullOrEmpty text then
        ""
    elif text.Length < 1 then
        text
    else
        // only remove quotes when at start AND end
        let startChar, endChar = text.[0], text.[text.Length - 1]

        match startChar, endChar with
        | '"', '"'
        | ''', ''' -> text.Substring(1, text.Length - 2)
        | _ -> text

let replaceDot (text: string) : string = text.Replace(".", "_")

let replaceAt (text: string) : string = text.Replace("@", "_AT_")

type SanitizeNameResult = { Name: string; IsDifferent: bool }

let sanitizeNameWithResult (name: string) : SanitizeNameResult =
    match name with
    // not allowed in type name even with backticks
    | "$type" -> { Name = "_Type"; IsDifferent = true }
    | _ ->
        let sanitizedName = name |> replaceDot |> replaceAt |> removeSurroundingQuotes

        // Check if the name is different after sanitization
        // This is used to check if the value is different from the default Fable computed value
        // Especially useful for StringEnum values where we sometimes need to use [<CompiledValue(...)>]
        // to provide a different value than the default Fable computed value from the name
        let isDifferent = name <> sanitizedName

        {
            Name = sanitizedName |> escapeName
            IsDifferent = isDifferent
        }

let sanitizeName (name: string) =
    let result = sanitizeNameWithResult name
    result.Name
