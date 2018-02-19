namespace FilenameEmbeddedMetadataOrganizer.Tests

open Xunit

[<AutoOpen>]
module Implementation =
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

    let rename parameters (originalFileName : string) : RenameResult =
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
            parameters.SelectedFeatures
            |> Option.map (fun features ->
                [
                    yield "["

                    yield! features |> List.sort

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
            DetectedFeatures = []
        }

module SplitFileNameTests =
    [<Theory>]
    [<InlineData("Aerial view over Uluru at night", "Aerial view over Uluru at night", "", "")>]
    [<InlineData("Aerial view (from above) over Uluru at night", "Aerial view (from above) over Uluru at night", "", "")>]
    [<InlineData("Aerial view over Uluru at night (Uluru)", "Aerial view over Uluru at night", "(Uluru)", "")>]
    [<InlineData("Aerial view over Uluru at night (.Andes.Pacific Ocean.)", "Aerial view over Uluru at night", "(.Andes.Pacific Ocean.)", "")>]
    [<InlineData("Aerial view (from above) over Uluru at night (.Andes.Pacific Ocean.)", "Aerial view (from above) over Uluru at night", "(.Andes.Pacific Ocean.)", "")>]
    [<InlineData("Aerial view over Uluru at night (.Andes.Pacific Ocean.) [.Ax.Bd.]", "Aerial view over Uluru at night", "(.Andes.Pacific Ocean.)", "[.Ax.Bd.]")>]
    [<InlineData("Aerial view over Uluru at night [.Ax.Bd.]", "Aerial view over Uluru at night", "", "[.Ax.Bd.]")>]
    let ``Filename parts are correctly detected`` (fileName, expectedMain, expectedNames, expectedFeatures) =
        // Arrange
        let fileName = fileName

        let expectedResult = (expectedMain, expectedNames, expectedFeatures)

        // Act
        let result = splitFileName true fileName

        // Assert
        Assert.Equal (expectedResult, result)

    [<Theory>]
    [<InlineData("Aerial view over Uluru at night", "Aerial view over Uluru at night", "", "")>]
    [<InlineData("Aerial view (from above) over Uluru at night", "Aerial view (from above) over Uluru at night", "", "")>]
    [<InlineData("Aerial view over Uluru at night (Uluru)", "Aerial view over Uluru at night (Uluru)", "", "")>]
    [<InlineData("Aerial view over Uluru at night (.Andes.Pacific Ocean.)", "Aerial view over Uluru at night (.Andes.Pacific Ocean.)", "", "")>]
    [<InlineData("Aerial view (from above) over Uluru at night (.Andes.Pacific Ocean.)", "Aerial view (from above) over Uluru at night (.Andes.Pacific Ocean.)", "", "")>]
    [<InlineData("Aerial view over Uluru at night (.Andes.Pacific Ocean.) [.Ax.Bd.]", "Aerial view over Uluru at night (.Andes.Pacific Ocean.)", "", "[.Ax.Bd.]")>]
    [<InlineData("Aerial view over Uluru at night [.Ax.Bd.]", "Aerial view over Uluru at night", "", "[.Ax.Bd.]")>]
    let ``Filename parts are correctly detected omitting separate name part`` (fileName, expectedMain, expectedNames, expectedFeatures) =
        // Arrange
        let fileName = fileName

        let expectedResult = (expectedMain, expectedNames, expectedFeatures)

        // Act
        let result = splitFileName false fileName

        // Assert
        Assert.Equal (expectedResult, result)

module RenameTests =
    let allNames =
        [
            "Uluru"
            "Great Barrier Reef"
            "Glasshouse Mountains"
            "Rocky Mountains"
            "Pacific Ocean"
        ]

    [<Fact>]
    let ``A single name is detected and written to metadata`` () =
        // Arrange
        let originalName = "Aerial view over Uluru at night"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = None
                TreatParenthesizedPartAsNames = false
            }

        let expectedResult =
            {
                NewFileName = "Aerial view over Uluru at night (.Uluru.)"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Two names are detected and written to metadata`` () =
        // Arrange
        let originalName = "View from the Glasshouse Mountains to the Great Barrier Reef"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = None
                TreatParenthesizedPartAsNames = false
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains to the Great Barrier Reef (.Glasshouse Mountains.Great Barrier Reef.)"
                DetectedNames = [ "Glasshouse Mountains"; "Great Barrier Reef" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Selected names override detected names`` () =
        // Arrange
        let originalName = "View from the Glasshouse Mountains to the Great Barrier Reef"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = Some [ "Rocky Mountains"; "Pacific Ocean" ]
                TreatParenthesizedPartAsNames = false
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains to the Great Barrier Reef (.Pacific Ocean.Rocky Mountains.)"
                DetectedNames = [ "Glasshouse Mountains"; "Great Barrier Reef" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Appended names override names in the main part`` () =
        // Arrange
        let originalName = "View from the Glasshouse Mountains to the Great Barrier Reef (Uluru, Pacific Ocean)"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = None
                TreatParenthesizedPartAsNames = true
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains to the Great Barrier Reef (.Pacific Ocean.Uluru.)"
                DetectedNames = [ "Pacific Ocean"; "Uluru" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Names in names part are reported regardless of prior existence`` () =
        // Arrange
        let originalName = "View from the Glasshouse Mountains to the Great Barrier Reef (Uluru, Andes)"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = None
                TreatParenthesizedPartAsNames = true
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains to the Great Barrier Reef (.Andes.Uluru.)"
                DetectedNames = [ "Andes"; "Uluru" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Treating parenthesized part as names can be disabled`` () =
        // Arrange
        let originalName = "Glasshouse Mountains at sunset (Glasshouse Mountains)"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = None
                TreatParenthesizedPartAsNames = false
            }

        let expectedResult =
            {
                NewFileName = "Glasshouse Mountains at sunset (Glasshouse Mountains) (.Glasshouse Mountains.)"
                DetectedNames = [ "Glasshouse Mountains" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``New appended names format replaces old format`` () =
        // Arrange
        let originalName = "Glasshouse Mountains at sunset (Glasshouse Mountains)"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = None
                TreatParenthesizedPartAsNames = true
            }

        let expectedResult =
            {
                NewFileName = "Glasshouse Mountains at sunset (.Glasshouse Mountains.)"
                DetectedNames = [ "Glasshouse Mountains" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``No selected or detected names cause no name part to be added`` () =
        // Arrange
        let originalName = "Here's that thing"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = None
                TreatParenthesizedPartAsNames = false
            }

        let expectedResult =
            {
                NewFileName = originalName
                DetectedNames = []
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Selected features are correctly appended`` () =
        // Arrange
        let originalName = "Aerial view over Uluru at night"

        let parameters =
            {
                SelectedFeatures = Some [ "Bd"; "Ax" ]
                AllNames = allNames
                SelectedNames = None
                TreatParenthesizedPartAsNames = false
            }

        let expectedResult =
            {
                NewFileName = "Aerial view over Uluru at night (.Uluru.) [.Ax.Bd.]"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)