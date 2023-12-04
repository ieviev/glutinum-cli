module rec Glutinum

(***)
#r "nuget: Fable.Core"
(***)

open Fable.Core
open System

[<RequireQualifiedAccess>]
[<StringEnum(CaseRules.None)>]
type ForegroundColor =
    | black
    | red
    | green

[<RequireQualifiedAccess>]
[<StringEnum(CaseRules.None)>]
type NoBlack =
    | red
    | green
