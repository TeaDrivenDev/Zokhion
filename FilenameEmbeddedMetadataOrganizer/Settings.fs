namespace FilenameEmbeddedMetadataOrganizer

[<AutoOpen>]
module Settings =
    open System
    open System.IO
    open System.Text.RegularExpressions

    type Feature =
        { Name : string; Code : string; Adds : string option; Instances : FeatureInstance list }
    and FeatureInstance = { Name : string; Code : string }

    type Replacement = { ToReplace : string; ReplaceWith : string }

    type Settings =
        {
            SourceDirectoryPrefixes : string
            DestinationDirectoryPrefixes : string
            Replacements : Replacement list
            Names : string list
            Features : Feature list
        }

    let prefixesFilePath directory = Path.Combine(directory, ".prefixes")
    let replacementsFilePath directory = Path.Combine(directory, ".replacements")
    let namesFilePath directory = Path.Combine(directory, ".names")
    let featuresFilePath directory = Path.Combine(directory, ".features")

    let writePrefixes directory (sourcePrefixes, destinationPrefixes) =
        let prefixesFilePath = prefixesFilePath directory

        let prefixesLines =
            [
                "Source", sourcePrefixes
                "Destination", destinationPrefixes
            ]
            |> List.filter (snd >> String.IsNullOrWhiteSpace >> not)
            |> List.map (uncurry (sprintf "%s=%s"))

        if not <| List.isEmpty prefixesLines || File.Exists prefixesFilePath
        then File.WriteAllLines(prefixesFilePath, prefixesLines)

    let readPrefixes directory =
        let prefixesFilePath = prefixesFilePath directory

        if File.Exists prefixesFilePath
        then
            let prefixes =
                File.ReadAllLines prefixesFilePath
                |> Array.map (fun s ->
                    let [| name; prefixes |] = s.Split [| '=' |]
                    name, prefixes)
                |> Map.ofArray

            let sourcePrefixes =
                prefixes |> Map.tryFind "Source" |> Option.defaultValue ""

            let destinationPrefixes =
                prefixes |> Map.tryFind "Destination" |> Option.defaultValue ""

            sourcePrefixes, destinationPrefixes
        else "", ""

    let writeReplacements directory replacements =
        let replacementsFilePath = replacementsFilePath directory

        let lines =
            replacements
            |> List.map (fun replacement ->
                sprintf "%s|%s" replacement.ToReplace replacement.ReplaceWith)

        if not <| List.isEmpty lines || File.Exists replacementsFilePath
        then File.WriteAllLines(replacementsFilePath, lines)

    let readReplacements directory =
        let replacementsFilePath = replacementsFilePath directory

        if File.Exists replacementsFilePath
        then
            File.ReadAllLines replacementsFilePath
            |> Array.map (fun s ->
                let [| toReplace; replaceWith |] = s.Split '|'

                { ToReplace = toReplace; ReplaceWith = replaceWith })
            |> Array.toList
        else []

    let writeNames directory (names : _ list) =
        let namesFilePath = namesFilePath directory

        if not <| List.isEmpty names || File.Exists namesFilePath
        then File.WriteAllLines(namesFilePath, names)

    let readNames directory =
        let namesFilePath = namesFilePath directory

        if File.Exists namesFilePath
        then File.ReadAllLines namesFilePath |> Array.toList
        else []

    let serializeFeatures features =
        let serializeInstance featureCode instance =
            sprintf "\t%s%s|%s"featureCode instance.Code instance.Name

        let serializeFeature (feature : Feature) =
            [
                yield
                    feature.Adds
                    |> Option.map (fun adds -> " > " + adds)
                    |> Option.defaultValue ""
                    |> sprintf "%s|%s%s" feature.Code feature.Name

                yield! feature.Instances |> List.map (serializeInstance feature.Code)
            ]

        features |> List.collect serializeFeature

    let writeFeatures directory (features : Feature list) =
        let featuresFilePath = featuresFilePath directory

        if not <| List.isEmpty features || File.Exists featuresFilePath
        then
            features
            |> serializeFeatures
            |> asSnd featuresFilePath
            |> File.WriteAllLines

    let instanceRegex = Regex "^\t(?<code>.+)\\|(?<name>.+)$"
    let featureRegex = Regex "^(?<code>.+)\\|(?<name>[^>]+)( > (?<adds>.+))?$"

    let deserializeFeatures serialized =
        let (|Instance|_|) featureCode s =
            let m = instanceRegex.Match s

            if m.Success
            then Some (m.Groups.["name"].Value, Regex.Replace(m.Groups.["code"].Value, "^" + featureCode, ""))
            else None

        let (|Feature|_|) s =
            let m = featureRegex.Match s

            if m.Success
            then Some (m.Groups.["name"].Value, m.Groups.["code"].Value, nonEmptyString m.Groups.["adds"].Value)
            else None

        let rec getInstances featureCode lines =
            match lines with
            | Instance featureCode (name, code) :: tail ->
                let instances, tail = getInstances featureCode tail
                (name, code) :: instances, tail
            | _ -> [], lines

        let rec deserialize lines =
            match lines with
            | Feature (name, code, adds) :: tail ->
                let instances, tail = getInstances code tail

                {
                    Name = name
                    Code = code
                    Adds = adds
                    Instances =
                        instances
                        |> List.map (fun (name, code) -> { Name = name; Code = code })
                }
                :: deserialize tail
            | _ -> []

        deserialize serialized

    let readFeatures directory =
        let featuresFilePath = featuresFilePath directory

        if File.Exists featuresFilePath
        then
            featuresFilePath
            |> File.ReadAllLines
            |> Array.toList
            |> deserializeFeatures
        else []

    let saveSettings baseDirectory settings =
        writePrefixes
            baseDirectory
            (settings.SourceDirectoryPrefixes, settings.DestinationDirectoryPrefixes)
        writeReplacements baseDirectory settings.Replacements
        writeNames baseDirectory settings.Names
        writeFeatures baseDirectory settings.Features

    let loadSettings baseDirectory =
        let sourcePrefixes, destinationPrefixes = readPrefixes baseDirectory

        {
            SourceDirectoryPrefixes = sourcePrefixes
            DestinationDirectoryPrefixes = destinationPrefixes
            Replacements = readReplacements baseDirectory
            Names = readNames baseDirectory
            Features = readFeatures baseDirectory
        }
