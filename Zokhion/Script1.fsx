#load "Logic.fs"

open System.IO
open System.Text.RegularExpressions

open TeaDriven.Zokhion

let path = @""

let features =
    readFeatures path
    |> List.collect (fun f ->
        f.Instances
        |> List.map (fun instance -> sprintf "%s%s" f.Code instance.Code))

let filePath = @""

let fileName = Path.GetFileNameWithoutExtension filePath
fileName

let splitFilePath path =
    Path.GetDirectoryName path, Path.GetFileNameWithoutExtension path, Path.GetExtension path

splitFilePath filePath

let splitFileName (name : string) =
    let m = Regex.Match(name, @"^(?<main>.*)\[\.(?<features>.+)\.\]$")

    if m.Success
    then Some (m.Groups.["main"].Value, (m.Groups.["features"].Value.Split('.') |> Array.map trim |> Array.toList))
    else None

splitFileName fileName

let processFile featureInstances filePath =
    let directory, fileName, extension = splitFilePath filePath

    fileName
    |> splitFileName
    |> Option.bind (fun (mainPart, features) ->
        let newFileName =
            (features, featureInstances)
            ||> join id id
            |> Seq.map fst
            |> String.concat "."
            |> sprintf "%s[.%s.]" mainPart

        let newFilePath = Path.Combine(directory, newFileName + extension)
        if newFilePath <> filePath
        then Some (filePath, newFilePath)
        else None)

Directory.GetFiles(path, "*.*", SearchOption.AllDirectories)
|> Seq.choose (processFile features)
|> Seq.iter File.Move
