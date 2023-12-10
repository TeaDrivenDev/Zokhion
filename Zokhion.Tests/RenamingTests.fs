namespace TeaDriven.Zokhion.Tests

module RenamingTests =
    open Xunit

    open TeaDriven.Zokhion
    open TeaDriven.Zokhion.RenamingTypes

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
            UnderscoreHandling = Ignore
            RecapitalizeNames = false
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
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``A name that is part of another detected name is not returned`` () =
        // Arrange
        let originalName = "View from the Glasshouse Mountains to the Great Barrier Reef"

        let parameters =
            {
                baseParameters with
                    AllNames = "Arri" :: baseParameters.AllNames
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains to the Great Barrier Reef (.Glasshouse Mountains.Great Barrier Reef.)"
                DetectedNames = [ "Glasshouse Mountains"; "Great Barrier Reef" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Theory>]
    [<InlineData("Mountains at dusk", false)>]
    [<InlineData("In the Mountains at dusk", false)>]
    [<InlineData("This is paramount", false)>]
    [<InlineData("Mount Kenya at dusk", true)>]
    [<InlineData("Near Mount Kenya at dusk", true)>]
    [<InlineData("Near the mount", true)>]
    let ``A detected name is only returned if it stands alone`` (originalName, expectedDetected) =
        // Arrange
        let nameToDetect = "Mount"

        let parameters =
            {
                baseParameters with
                    AllNames = nameToDetect :: baseParameters.AllNames
            }

        // Act
        let { DetectedNames = detectedNames } = Renaming.rename parameters originalName

        // Assert
        Assert.Equal (detectedNames |> List.contains nameToDetect, expectedDetected)

    [<Fact>]
    let ``A selected name that is part of another returned name is returned`` () =
        // Arrange
        let originalName = "View over the Great Barrier Reef (.Arri.Great Barrier Reef.)"

        let parameters = baseParameters

        let expectedResult =
            {
                NewFileName = "View over the Great Barrier Reef (.Arri.Great Barrier Reef.)"
                DetectedNames = [ "Arri"; "Great Barrier Reef" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Detected existing names are properly capitalized`` () =
        // Arrange
        let originalName = "View from mount mackenzie across the rainbow range (mount mackenzie, rainbow range)"

        let parameters = baseParameters

        let expectedResult =
            {
                NewFileName = "View from mount mackenzie across the rainbow range (.Mount MacKenzie.rainbow range.)"
                DetectedNames = [ "Mount MacKenzie"; "rainbow range" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Detected new names are not automatically capitalized`` () =
        // Arrange
        let originalName = "View from mount mcloughlin across the cascades range (Mount McLoughlin, cascades range)"

        let parameters = baseParameters

        let expectedResult =
            {
                NewFileName = "View from mount mcloughlin across the cascades range (.cascades range.Mount McLoughlin.)"
                DetectedNames = [ "cascades range"; "Mount McLoughlin" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Selected new names can change capitalization`` () =
        // Arrange
        let originalName = "View from mount mackenzie across the rainbow range (mount mackenzie, rainbow range)"

        let parameters =
            {
                baseParameters with
                    SelectedNames = Some [ "Mount MacKenzie"; "rainbow range" ]
                    RecapitalizeNames = true
            }

        let expectedResult =
            {
                NewFileName = "View from mount mackenzie across the rainbow range (.Mount MacKenzie.Rainbow Range.)"
                DetectedNames = [ "Mount MacKenzie"; "Rainbow Range" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Names in main part can be fixed up with proper capitalization`` () =
        // Arrange
        let originalName = "View from mount mackenzie across the rainbow range (mount mackenzie, rainbow range)"

        let parameters =
            {
                baseParameters with
                    RecapitalizeNames = true
                    FixupNamesInMainPart = true
            }

        let expectedResult =
            {
                NewFileName = "View from Mount MacKenzie across the Rainbow Range (.Mount MacKenzie.Rainbow Range.)"
                DetectedNames = [ "Mount MacKenzie"; "Rainbow Range" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Detected names can be re-selected`` () =
        // Arrange
        let originalName = "View from the Glasshouse Mountains to the Great Barrier Reef (Denali)"

        let parameters =
            {
                baseParameters with
                    SelectedNames = Some [ "Rocky Mountains"; "Pacific Ocean"; "Denali" ]
            }

        let expectedResult =
            {
                NewFileName = "View from the Glasshouse Mountains to the Great Barrier Reef (.Denali.Pacific Ocean.Rocky Mountains.)"
                DetectedNames = [ "Denali"; "Pacific Ocean"; "Rocky Mountains" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``A marked names part is always treated as names`` () =
        // Arrange
        let originalName = "Here is a name (.Mount Toba.Uluru.) [.XAb.]"

        let parameters =
            {
                baseParameters with
                    TreatParenthesizedPartAsNames = false
            }

        let expectedResult =
            {
                NewFileName = "Here is a name (.Mount Toba.Uluru.) [.XAb.]"
                DetectedNames = [ "Mount Toba"; "Uluru" ]
                DetectedFeatures = [ "XAb" ]
            }

        // Act
        let actualResult = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual(expectedResult, actualResult)

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
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

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
                NewFileName = "Aerial view over Uluru at night (.Uluru.) [.Bd.Ax.]"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = [ "Bd"; "Ax" ]
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Features are correctly detected`` () =
        // Arrange
        let originalName = "Aerial view over Uluru at night [.Ax.Xx.Bd.]"

        let parameters = baseParameters

        let expectedResult =
            {
                NewFileName = "Aerial view over Uluru at night (.Uluru.) [.Ax.Xx.Bd.]"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = [ "Ax"; "Xx"; "Bd" ]
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Features can be added`` () =
        // Arrange
        let originalName = "Aerial view over Uluru at night [.Ax.Xx.]"

        let parameters =
            {
                baseParameters with
                    SelectedFeatures = Some [ "Ax"; "Xx"; "Bd" ]
            }

        let expectedResult =
            {
                NewFileName = "Aerial view over Uluru at night (.Uluru.) [.Ax.Xx.Bd.]"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = [ "Ax"; "Xx"; "Bd" ]
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``An empty selected features list is omitted`` () =
        // Arrange
        let originalName = "Aerial view over Uluru at night [.Ax.Xx.]"

        let parameters =
            {
                baseParameters with
                    SelectedFeatures = Some []
            }

        let expectedResult =
            {
                NewFileName = "Aerial view over Uluru at night (.Uluru.)"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Square brackets without feature pattern are left alone`` () =
        // Arrange
        let originalName = "Aerial view over Uluru at night [NoFeature]"

        let parameters = baseParameters

        let expectedResult =
            {
                NewFileName = "Aerial view over Uluru at night [NoFeature] (.Uluru.)"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

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
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Replacing underscores works`` () =
        // Arrange
        let originalName = "Here_is_the_thing"

        let parameters =
            {
                baseParameters with
                    UnderscoreHandling = Replace
            }

        let expectedResult =
            {
                NewFileName = "Here is the thing"
                DetectedNames = []
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Trimming an underscore suffix works`` () =
        // Arrange
        let originalName = "Here is a name_with suffix"

        let parameters =
            {
                baseParameters with
                    UnderscoreHandling = TrimSuffix
            }

        let expectedResult =
            {
                NewFileName = "Here is a name"
                DetectedNames = []
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Trimming an underscore suffix also replaces other underscores`` () =
        // Arrange
        let originalName = "Here_is_a name_with suffix"

        let parameters =
            {
                baseParameters with
                    UnderscoreHandling = TrimSuffix
            }

        let expectedResult =
            {
                NewFileName = "Here is a name"
                DetectedNames = []
                DetectedFeatures = []
            }

        // Act
        let result = Renaming.rename parameters originalName

        // Assert
        Assert.StrictEqual (expectedResult, result)
