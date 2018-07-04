namespace FilenameEmbeddedMetadataOrganizer

[<AutoOpen>]
module Prelude =
    open System.Linq

    type FullJoinResult<'TLeft, 'TRight> =
        | LeftOnly of 'TLeft
        | RightOnly of 'TRight
        | JoinMatch of 'TLeft * 'TRight

    [<AllowNullLiteral>]
    type JoinWrapper<'a>(value : 'a) =
        member __.Value = value

    let (|JoinWrapped|) (wrapper : JoinWrapper<_>) = wrapper.Value

    let asFst second first = first, second
    let asSnd first second = first, second

    let swap (a, b) = b, a

    let uncurry fn (a, b) = fn a b

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

    let (|OfNull|) value = if isNull value then None else Some value

module Interop =
    open System
    open System.Runtime.InteropServices

    // see https://www.pinvoke.net/default.aspx/shell32/ShellExecuteEx.html
    [<StructLayout(LayoutKind.Sequential)>]
    [<Struct>]
    type SHELLEXECUTEINFO =
        val mutable cbSize : int
        val mutable fMask : uint32
        val mutable hwnd : IntPtr
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpVerb : string
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpFile : string
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpParameters : string
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpDirectory : string
        val mutable nShow : int
        val mutable hInstApp : IntPtr
        val mutable lpIDList : IntPtr
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpClass : string
        val mutable hkeyClass : IntPtr
        val mutable dwHotKey : uint32
        val mutable hIcon : IntPtr
        val mutable hProcess : IntPtr

    [<DllImport("shell32.dll", CharSet = CharSet.Auto)>]
    extern bool ShellExecuteEx(SHELLEXECUTEINFO& lpExecInfo);

    type ShowCommands =
        | SW_HIDE = 0
        | SW_SHOWNORMAL = 1
        | SW_NORMAL = 1
        | SW_SHOWMINIMIZED = 2
        | SW_SHOWMAXIMIZED = 3
        | SW_MAXIMIZE = 3
        | SW_SHOWNOACTIVATE = 4
        | SW_SHOW = 5
        | SW_MINIMIZE = 6
        | SW_SHOWMINNOACTIVE = 7
        | SW_SHOWNA = 8
        | SW_RESTORE = 9
        | SW_SHOWDEFAULT = 10
        | SW_FORCEMINIMIZE = 11
        | SW_MAX = 1

    [<Flags>]
    type ShellExecuteMaskFlags =
        | SEE_MASK_DEFAULT = 0x00000000u
        | SEE_MASK_CLASSNAME = 0x00000001u
        | SEE_MASK_CLASSKEY = 0x00000003u
        | SEE_MASK_IDLIST = 0x00000004u
        | SEE_MASK_INVOKEIDLIST = 0x0000000cu // Note SEE_MASK_INVOKEIDLIST(0xC) implies SEE_MASK_IDLIST(0x04)
        | SEE_MASK_HOTKEY = 0x00000020u
        | SEE_MASK_NOCLOSEPROCESS = 0x00000040u
        | SEE_MASK_CONNECTNETDRV = 0x00000080u
        | SEE_MASK_NOASYNC = 0x00000100u
        | SEE_MASK_FLAG_DDEWAIT = 0x00000100u
        | SEE_MASK_DOENVSUBST = 0x00000200u
        | SEE_MASK_FLAG_NO_UI = 0x00000400u
        | SEE_MASK_UNICODE = 0x00004000u
        | SEE_MASK_NO_CONSOLE = 0x00008000u
        | SEE_MASK_ASYNCOK = 0x00100000u
        | SEE_MASK_HMONITOR = 0x00200000u
        | SEE_MASK_NOZONECHECKS = 0x00800000u
        | SEE_MASK_NOQUERYCLASSSTORE = 0x01000000u
        | SEE_MASK_WAITFORINPUTIDLE = 0x02000000u
        | SEE_MASK_FLAG_LOG_USAGE = 0x04000000u

    let showFileProperties fileName =
        let mutable info = SHELLEXECUTEINFO()
        info.cbSize <- Marshal.SizeOf(info)
        info.lpVerb <- "properties"
        info.lpFile <- fileName
        info.nShow <- int ShowCommands.SW_SHOW
        info.fMask <- uint32 ShellExecuteMaskFlags.SEE_MASK_INVOKEIDLIST
        ShellExecuteEx(&info);

[<AutoOpen>]
module Settings =
    open System
    open System.IO
    open System.Text.RegularExpressions

    type Feature = { Name : string; Code : string; Instances : FeatureInstance list }
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
                yield sprintf "%s|%s"feature.Code feature.Name

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
    let featureRegex = Regex "^(?<code>.+)\\|(?<name>.+)$"

    let deserializeFeatures serialized =
        let (|Instance|_|) featureCode s =
            let m = instanceRegex.Match s

            if m.Success
            then Some (m.Groups.["name"].Value, Regex.Replace(m.Groups.["code"].Value, "^" + featureCode, ""))
            else None

        let (|Feature|_|) s =
            let m = featureRegex.Match s

            if m.Success
            then Some (m.Groups.["name"].Value, m.Groups.["code"].Value)
            else None

        let rec getInstances featureCode lines =
            match lines with
            | Instance featureCode (name, code) :: tail ->
                let instances, tail = getInstances featureCode tail
                (name, code) :: instances, tail
            | _ -> [], lines

        let rec deserialize lines =
            match lines with
            | Feature (name, code) :: tail ->
                let instances, tail = getInstances code tail

                {
                    Name = name
                    Code = code
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
        |> List.map (fun name -> name, toUpper name)
        |> List.filter (snd >> part.Contains)
        |> List.map (fun (name, normalized) ->
            {
                Name = name
                Normalized = normalized
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
