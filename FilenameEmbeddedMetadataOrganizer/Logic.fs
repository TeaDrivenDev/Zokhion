namespace FilenameEmbeddedMetadataOrganizer

[<AutoOpen>]
module Prelude =
    open System.Linq

    type FullJoinResult<'TLeft, 'TRight> =
        | LeftOnly of 'TLeft
        | RightOnly of 'TRight
        | JoinMatch of 'TLeft * 'TRight

    let asFst second first = first, second
    let asSnd first second = first, second

    let swap (a, b) = b, a

    let leftJoin innerKeySelector outerKeySelector (inner : seq<'TInner>) (outer : seq<'TOuter>) =
        query {
            for o in outer do
            leftOuterJoin i in inner on
                (outerKeySelector o = innerKeySelector i) into result
            for joined in result.DefaultIfEmpty() do
            select (o, joined |> Option.ofObj)
        }

    let fullOuterJoin innerKeySelector outerKeySelector (right : 'TRight seq) (left : 'TLeft seq) =
        let optionizeFirst (a, b) = Some a, b

        let valueInOuter =
            leftJoin innerKeySelector outerKeySelector right left
            |> Seq.map optionizeFirst

        let valueInInnerOnly =
            leftJoin outerKeySelector innerKeySelector left right
            |> Seq.filter (snd >> Option.isNone)
            |> Seq.map (optionizeFirst >> swap)

        Seq.append valueInOuter valueInInnerOnly
        |> Seq.map (function
            | Some leftItem, Some rightItem -> JoinMatch (leftItem, rightItem)
            | Some leftItem, None -> LeftOnly leftItem
            | None, Some rightItem -> RightOnly rightItem
            | None, None -> failwith "This can never happen.")

[<AutoOpen>]
module Logic =
    open System
    open System.Globalization
    open System.Text.RegularExpressions

    let inline (<||>) f g x = f x || g x
    let trim (s : string) = s.Trim()
    let toUpper (s : string) = s.ToUpper()
    let toTitleCase s = CultureInfo.CurrentCulture.TextInfo.ToTitleCase s

    type RenameParameters =
        {
            SelectedFeatures : string list option
            AllNames : string list
            SelectedNames : string list option
            TreatParenthesizedPartAsNames : bool
            DetectNamesInMainAndNamesParts : bool
            FixupNamesInMainPart : bool
            ReplaceUnderscores : bool
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

    let detectListedNames (allNames : string list) (part : string) =
        let part = part.ToUpper()

        allNames |> List.filter (toUpper >> part.Contains)

    let rename parameters (originalFileName : string) : RenameResult =
        let originalFileName = string originalFileName

        let originalFileName =
            let originalFileName =
                if parameters.ReplaceUnderscores
                then originalFileName.Replace("_", " ")
                else originalFileName

            match parameters.Replacements with
            | [] -> originalFileName
            | replacements ->
                (originalFileName, replacements)
                ||> List.fold (fun acc (replace, replaceWith) -> acc.Replace(replace, replaceWith))

        let (mainPart, namesPart, featuresPart) =
            splitFileName parameters.TreatParenthesizedPartAsNames originalFileName

        let detectedNames =
            evaluateNamesPart namesPart
            |> Option.map (fun names ->
                if parameters.DetectNamesInMainAndNamesParts
                then names @ detectListedNames parameters.AllNames mainPart
                else names)
            |> Option.defaultWith (fun () -> detectListedNames parameters.AllNames mainPart)
            |> asFst parameters.AllNames
            ||> fullOuterJoin toUpper toUpper
            |> Seq.choose (function
                | LeftOnly _ -> None
                | RightOnly detected -> detected |> toTitleCase |> Some
                | JoinMatch (listed, detected) -> Some listed)
            |> Seq.distinct
            |> Seq.toList
            |> List.sort

        let mainPart =
            if parameters.FixupNamesInMainPart
            then
                (mainPart, detectedNames)
                ||> List.fold (fun acc current ->
                    Regex.Replace(acc, current, current, RegexOptions.IgnoreCase))
            else mainPart

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
