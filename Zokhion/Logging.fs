namespace TeaDriven.Zokhion

open System

module LoggingTypes =
    type LogLevel =
        | Diagnostic
        | Informational
        | Warning
        | Critical

    type Activity =
        | KeepAlive
        | RenameSuccess
        | RenameFailure
        | DeleteSuccess
        | DeleteFailure
        | AddName

    type LogEntry =
        {
            Timestamp: DateTimeOffset
            LogLevel: LogLevel
            Activity: Activity
            Message: string
        }

[<RequireQualifiedAccess>]
module Logging =
    open LoggingTypes

    let log writeLog logLevel activity message =
        {
            Timestamp = DateTimeOffset.Now
            LogLevel = logLevel
            Activity = activity
            Message = message
        }
        |> writeLog
