namespace FilenameEmbeddedMetadataOrganizer

[<AutoOpen>]
module Logic =
    open System
    open System.Text.RegularExpressions

    let inline (<||>) f g x = f x || g x
    let trim (s : string) = s.Trim()

    type RenameParameters =
        {
            SelectedFeatures : string list option
            AllNames : string list
            SelectedNames : string list option
            TreatParenthesizedPartAsNames : bool
            Replacements : (string * string) list
        }

    type RenameResult =
        {
            NewFileName : string
            DetectedFeatures : string list
            DetectedNames : string list
        }

    let splitFileName preserveSeparateNamesPart (fileName : string) =
        let regex =
            if preserveSeparateNamesPart
            then @"^(?<main>.+?)\s*(?<names>\([^\)]+\))?\s*(?<features>\[.+\])?$"
            else @"^(?<main>.+?)\s*(?<features>\[.+\])?$"
            |> Regex

        let m = regex.Match fileName

        if m.Success
        then (m.Groups.["main"].Value, m.Groups.["names"].Value, m.Groups.["features"].Value)
        else "", "", ""

    let evaluateNamesPart (names : string) =
        let m = Regex.Match(names, @"^\(\.(?<names>.+)\.\)$")

        if m.Success
        then Some (m.Groups.["names"].Value.Split('.') |> Array.map trim |> Array.toList)
        else
            let m = Regex.Match(names, @"^\((?<names>.+)\)")

            if m.Success
            then Some (m.Groups.["names"].Value.Split(',') |> Array.map trim |> Array.toList)
            else None

    let evaluateFeaturesPart (names : string) =
        let m = Regex.Match(names, @"^\[\.(?<features>.+)\.\]$")

        if m.Success
        then Some (m.Groups.["features"].Value.Split('.') |> Array.map trim |> Array.toList)
        else None

    let rename parameters (originalFileName : string) : RenameResult =
        let originalFileName =
            match parameters.Replacements with
            | [] -> originalFileName
            | replacements ->
                (originalFileName, replacements)
                ||> List.fold (fun acc (replace, replaceWith) -> acc.Replace(replace, replaceWith))

        let (mainPart, namesPart, featuresPart) =
            splitFileName parameters.TreatParenthesizedPartAsNames originalFileName

        let detectedNames =
            evaluateNamesPart namesPart
            |> Option.defaultWith (fun _ -> parameters.AllNames |> List.filter mainPart.Contains)
            |> List.sort

        let namesToUse =
            parameters.SelectedNames
            |> Option.defaultValue detectedNames
            |> List.sort

        let detectedFeatures =
            evaluateFeaturesPart featuresPart
            |> Option.map List.sort

        let featuresToUse =
            detectedFeatures
            |> Option.orElse parameters.SelectedFeatures
            |> Option.map List.sort

        let names =
            match namesToUse with
            | [] -> ""
            | _ ->
                [
                    yield "("

                    yield! namesToUse

                    yield ")"
                ]
                |> String.concat "."

        let features =
            featuresToUse
            |> Option.map (fun features ->
                [
                    yield "["

                    yield! features

                    yield "]"
                ]
                |> String.concat ".")
            |> Option.defaultValue ""

        let newFileName =
            [ mainPart; names; features ]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat " "

        {
            NewFileName = newFileName
            DetectedNames = detectedNames
            DetectedFeatures = detectedFeatures |> Option.defaultValue []
        }