namespace FilenameEmbeddedMetadataOrganizer.Tests

open Xunit

module Tests =
    open System

    type RenameParameters =
        {
            SelectedFeatures : string list option
            AllNames : string list
            SelectedNames : string list option
            ReplaceExistingNames : bool
        }

    type RenameResult =
        {
            NewFileName : string
            DetectedFeatures : string list
            DetectedNames : string list
        }

    let rename parameters (originalFileName : string) : RenameResult =
        let detectedNames =
            parameters.AllNames
            |> List.filter originalFileName.Contains
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
            [ originalFileName; names; features ]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat " "

        {
            NewFileName = newFileName
            DetectedNames = detectedNames
            DetectedFeatures = []
        }

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
                ReplaceExistingNames = false
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
                ReplaceExistingNames = false
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
                ReplaceExistingNames = false
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
    let ``No selected or detected names cause no name part to be added`` () =
        // Arrange
        let originalName = "Here's that thing"

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = None
                ReplaceExistingNames = false
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
                ReplaceExistingNames = false
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