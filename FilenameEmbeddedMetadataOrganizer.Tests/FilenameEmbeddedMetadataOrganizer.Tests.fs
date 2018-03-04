namespace FilenameEmbeddedMetadataOrganizer.Tests

open Xunit

open FilenameEmbeddedMetadataOrganizer

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
            "Mount MacKenzie"
        ]

    let baseParameters =
        {
            SelectedFeatures = None
            AllNames = allNames
            SelectedNames = None
            TreatParenthesizedPartAsNames = true
            DetectNamesInMainAndNamesParts = false
            FixupNamesInMainPart = false
            ReplaceUnderscores = false
            Replacements = []
        }

    [<Fact>]
    let ``A single name is detected and written to metadata`` () =
        // Arrange
        let originalName = "Aerial view over Uluru at night"

        let parameters = baseParameters

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

        let parameters = baseParameters

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
    let ``Names are detected regardless of case`` () =
        // Arrange
        let originalName = "View from the glasshouse mountains to the great barrier reef"

        let parameters = baseParameters

        let expectedResult =
            {
                NewFileName = "View from the glasshouse mountains to the great barrier reef (.Glasshouse Mountains.Great Barrier Reef.)"
                DetectedNames = [ "Glasshouse Mountains"; "Great Barrier Reef" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Detected names are properly capitalized`` () =
        // Arrange
        let originalName = "View from mount mackenzie across the rainbow range (mount mackenzie, rainbow range)"

        let parameters = baseParameters

        let expectedResult =
            {
                NewFileName = "View from mount mackenzie across the rainbow range (.Mount MacKenzie.Rainbow Range.)"
                DetectedNames = [ "Mount MacKenzie"; "Rainbow Range" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Names in main part can be fixed up with proper capitalization`` () =
        // Arrange
        let originalName = "View from mount mackenzie across the rainbow range (mount mackenzie, rainbow range)"

        let parameters =
            {
                baseParameters with
                    FixupNamesInMainPart = true
            }

        let expectedResult =
            {
                NewFileName = "View from Mount MacKenzie across the Rainbow Range (.Mount MacKenzie.Rainbow Range.)"
                DetectedNames = [ "Mount MacKenzie"; "Rainbow Range" ]
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
                baseParameters with
                    SelectedNames = Some [ "Rocky Mountains"; "Pacific Ocean" ]
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains to the Great Barrier Reef (.Pacific Ocean.Rocky Mountains.)"
                DetectedNames = [ "Pacific Ocean"; "Rocky Mountains" ]
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

        let parameters = baseParameters

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
    let ``Names can be detected in main part when a names part exists`` () =
        // Arrange
        let originalName = "View from the Glasshouse Mountains (Uluru)"

        let parameters =
            {
                baseParameters with
                    DetectNamesInMainAndNamesParts = true
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains (.Glasshouse Mountains.Uluru.)"
                DetectedNames = [ "Glasshouse Mountains"; "Uluru" ]
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Names in both main and names part are reported only once`` () =
        // Arrange
        let originalName = "View from the Glasshouse Mountains (Glasshouse Mountains, Uluru)"

        let parameters =
            {
                baseParameters with
                    DetectNamesInMainAndNamesParts = true
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains (.Glasshouse Mountains.Uluru.)"
                DetectedNames = [ "Glasshouse Mountains"; "Uluru" ]
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

        let parameters = baseParameters

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
                baseParameters with
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

        let parameters = baseParameters

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

        let parameters = baseParameters

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
    let ``Replacements are replaced correctly`` () =
        // Arrange
        let originalName = "PHOTO... Uluru at night - DigitalCam (.Uluru.)"

        let parameters =
            {
                baseParameters with
                    Replacements = [ "PHOTO...", "Photo -"; "- DigitalCam ", "" ]
            }

        let expectedResult =
            {
                NewFileName = "Photo - Uluru at night (.Uluru.)"
                DetectedNames = [ "Uluru" ]
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
                baseParameters with
                    SelectedFeatures = Some [ "Bd"; "Ax" ]
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

    [<Fact>]
    let ``Features are correctly detected`` () =
        // Arrange
        let originalName = "Aerial view over Uluru at night [.Ax.Xx.Bd.]"

        let parameters = baseParameters

        let expectedResult =
            {
                NewFileName = "Aerial view over Uluru at night (.Uluru.) [.Ax.Bd.Xx.]"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = [ "Ax"; "Bd"; "Xx" ]
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Underscores are not replaced by default`` () =
        // Arrange
        let originalName = "Here_is_the_thing"

        let parameters = baseParameters

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
    let ``Replacing underscores works`` () =
        // Arrange
        let originalName = "Here_is_the_thing"

        let parameters =
            {
                baseParameters with
                    ReplaceUnderscores = true
            }

        let expectedResult =
            {
                NewFileName = "Here is the thing"
                DetectedNames = []
                DetectedFeatures = []
            }

        // Act
        let result = rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)
