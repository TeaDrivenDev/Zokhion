namespace FilenameEmbeddedMetadataOrganizer

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
            RecapitalizeNames : bool
            ReplaceUnderscores : bool
            Replacements : (string * string) list
        }

    type OptionChange =
        | TreatParenthesizedPartAsNames of bool
        | FixupNamesInMainPart of bool
        | RecapitalizeNames of bool
        | ReplaceUnderscores of bool
        | DetectNamesInMainAndNamesParts of bool
        | SelectedNames of string list option
        | SelectedFeatures of string list option
        | ResetSelections

    let updateParameters replacements getAllNames parameters change =
        let parameters =
            {
                parameters with
                    AllNames = getAllNames ()
                    Replacements =
                        replacements
                        |> Seq.map (fun replace -> replace.ToReplace, replace.ReplaceWith)
                        |> Seq.toList
            }

        match change with
        | TreatParenthesizedPartAsNames value ->
            {
                parameters with
                    TreatParenthesizedPartAsNames = value
                    SelectedNames = None
            }
        | FixupNamesInMainPart value ->
            { parameters with FixupNamesInMainPart = value }
        | RecapitalizeNames value ->
            { parameters with RecapitalizeNames = value }
        | ReplaceUnderscores value ->
            { parameters with ReplaceUnderscores = value }
        | DetectNamesInMainAndNamesParts value ->
            {
                parameters with
                    DetectNamesInMainAndNamesParts = value
                    SelectedNames = None
            }
        | SelectedNames value ->
            { parameters with SelectedNames = value }
        | SelectedFeatures value ->
            { parameters with SelectedFeatures = value }
        | ResetSelections ->
            {
                parameters with
                    SelectedFeatures = None
                    SelectedNames = None
            }

    type RenameResult =
        {
            NewFileName : string
            DetectedFeatures : string list
            DetectedNames : string list
        }

    type NameSource = Selected | NamesPart | MainPart

    type MatchedName =
        {
            Name : string
            Normalized : string
            Source : NameSource
        }

    let splitFileName preserveSeparateNamesPart (fileName : string) =
        let regex =
            if preserveSeparateNamesPart
            then @"^(?<main>.+?)\s*(?<names>\([^\)]+\))?\s*(?<features>\[\..+\.\])?$"
            else @"^(?<main>.+?)\s*(?<features>\[\..+\.\])?$"
            |> Regex

        let m = regex.Match fileName

        if m.Success
        then (m.Groups.["main"].Value, m.Groups.["names"].Value, m.Groups.["features"].Value)
        else "", "", ""

    let evaluateNamesPart (names : string) =
        let splitBy separator (``match`` : Match) =
            ``match``.Groups.["names"].Value.Split separator
            |> Array.map trim
            |> Array.toList
            |> List.map (fun name ->
                {
                    Name = name
                    Normalized = toUpper name
                    Source = NamesPart
                })

        let m = Regex.Match(names, @"^\(\.(?<names>.+)\.\)$")

        if m.Success
        then Some (splitBy [| '.' |] m)
        else
            let m = Regex.Match(names, @"^\((?<names>.+)\)")

            if m.Success
            then Some (splitBy [| ',' |] m)
            else None

    let evaluateFeaturesPart (names : string) =
        let m = Regex.Match(names, @"^\[\.(?<features>.+)\.\]$")

        if m.Success
        then Some (m.Groups.["features"].Value.Split('.') |> Array.map trim |> Array.toList)
        else None

    let detectListedNames (allNames : string list) source (part : string) =
        let part = part.ToUpper()

        allNames
        |> List.filter (fun name ->
            Regex.IsMatch(part, sprintf @"\b%s\b" name, RegexOptions.IgnoreCase) )
        |> List.map (fun name ->
            {
                Name = name
                Normalized = toUpper name
                Source = source
            })

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
                then names @ detectListedNames parameters.AllNames MainPart mainPart
                else names)
            |> Option.defaultWith (fun () -> detectListedNames parameters.AllNames MainPart mainPart)
            |> List.map JoinWrapper
            |> asFst parameters.AllNames
            ||> fullOuterJoin (fun n -> n.Value.Normalized) toUpper
            |> Seq.choose (function
                | LeftOnly _ -> None
                | RightOnly (JoinWrapped detected) ->
                    if parameters.RecapitalizeNames
                    then { detected with Name = toTitleCase detected.Name }
                    else detected
                    |> Some
                | JoinMatch (listed, JoinWrapped detected) -> Some { detected with Name = listed })
            |> Seq.distinct
            |> Seq.toList
            |> List.sort

        let detectedAndSelectedNames =
            parameters.SelectedNames
            |> Option.map (fun selected ->
                selected
                |> List.map (fun name ->
                    { Name = name; Normalized = toUpper name; Source = Selected }
                    |> JoinWrapper)
                |> asSnd parameters.AllNames
                ||> leftJoin toUpper (fun n -> n.Value.Normalized)
                |> Seq.map (function
                    | JoinWrapped selected, Some inAll -> selected
                    | JoinWrapped selected, None ->
                        detectedNames
                        |> List.tryFind (fun detected -> detected.Normalized = selected.Normalized)
                        |> Option.map (fun detected -> { selected with Name = detected.Name })
                        |> Option.defaultValue selected)
                |> Seq.toList)
            |> Option.defaultValue detectedNames
            |> List.groupBy (fun n -> n.Normalized)
            |> List.map (fun (_, occurrences) ->
                occurrences |> List.sortBy (fun n -> n.Source) |> List.head)
            |> List.sortBy (fun n -> n.Normalized)

        let namesToUse =
            detectedAndSelectedNames
            |> List.filter (fun name ->
                name.Source <> MainPart
                || detectedAndSelectedNames
                   |> List.forall (fun n ->
                        name = n || n.Normalized.Contains name.Normalized |> not))
            |> List.map (fun n -> n.Name)

        let mainPart =
            if parameters.FixupNamesInMainPart
            then
                (mainPart, namesToUse)
                ||> List.fold (fun acc current ->
                    Regex.Replace(acc, current, current, RegexOptions.IgnoreCase))
            else mainPart

        let detectedFeatures =
            evaluateFeaturesPart featuresPart

        let featuresToUse =
            parameters.SelectedFeatures
            |> Option.orElse detectedFeatures

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
                match features with
                | [] -> ""
                | _ ->
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
            DetectedNames = namesToUse
            DetectedFeatures = featuresToUse |> Option.defaultValue []
        }
