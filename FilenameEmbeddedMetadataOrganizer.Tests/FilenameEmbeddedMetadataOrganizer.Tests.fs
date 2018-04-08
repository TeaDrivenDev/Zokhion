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
        let result = rename parameters originalName

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
        let result = rename parameters originalName

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
                DetectedFeatures = [ "Ax"; "Bd" ]
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
                NewFileName = "Aerial view over Uluru at night (.Uluru.) [.Ax.Bd.Xx.]"
                DetectedNames = [ "Uluru" ]
                DetectedFeatures = [ "Ax"; "Bd"; "Xx" ]
            }

        // Act
        let result = rename parameters originalName

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

module SettingsTests =
    [<Fact>]
    let ``A feature hierarchy is serialized in the correct format`` () =
        // Arrange
        let values =
            [
                {
                    Name = "Scene"
                    Code = "S"
                    Instances =
                        [
                            { Name = "Sunset"; Code = "Ss" }
                            { Name = "Pre-dawn"; Code = "Pd" }
                        ]
                }

                {
                    Name = "Type of shot"
                    Code = "T"
                    Instances =
                        [
                            { Name = "Aerial, distant"; Code = "Ad" }
                            { Name = "Aerial, overhead"; Code = "Ao" }
                            { Name = "Ground level, distant"; Code = "Gd" }
                        ]
                }
            ]

        let expectedResult =
            [
                "S|Scene"
                "	SSs|Sunset"
                "	SPd|Pre-dawn"
                "T|Type of shot"
                "	TAd|Aerial, distant"
                "	TAo|Aerial, overhead"
                "	TGd|Ground level, distant"
            ]

        // Act
        let result : string list = serializeFeatures values

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``A feature list without instances is correctly deserialized`` () =
        // Arrange
        let values =
            [
                "S|Scene"
                "T|Type of shot"
            ]

        let expectedResult =
            [
                { Name = "Scene"; Code = "S"; Instances = [] }
                { Name = "Type of shot"; Code = "T"; Instances = [] }
            ]

        // Act
        let result = deserializeFeatures values

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``A feature hierarchy is correctly deserialized`` () =
        // Arrange
        let values =
            [
                "S|Scene"
                "	SSs|Sunset"
                "	SPd|Pre-dawn"
                "T|Type of shot"
                "	TAd|Aerial, distant"
                "	TAo|Aerial, overhead"
                "	TGd|Ground level, distant"
            ]

        let expectedResult =
            [
                {
                    Name = "Scene"
                    Code = "S"
                    Instances =
                        [
                            { Name = "Sunset"; Code = "Ss" }
                            { Name = "Pre-dawn"; Code = "Pd" }
                        ]
                }

                {
                    Name = "Type of shot"
                    Code = "T"
                    Instances =
                        [
                            { Name = "Aerial, distant"; Code = "Ad" }
                            { Name = "Aerial, overhead"; Code = "Ao" }
                            { Name = "Ground level, distant"; Code = "Gd" }
                        ]
                }
            ]

        // Act
        let result = deserializeFeatures values

        // Assert
        Assert.StrictEqual (expectedResult, result)

module GenerateCodeTests =
    open System

    type FillupWithVowels = DoNotFillup | FillFromStart | FillFromEnd

    type GenerateCodeParameters =
        {
            CodeLength : int
            PreferConsonants : FillupWithVowels option
            KeepAdditionalCapitalLetters : bool
        }

    let baseParameters =
        {
            CodeLength = 2
            PreferConsonants = None
            KeepAdditionalCapitalLetters = true
        }

    let isVowel (c : char) = c |> string |> "aeiou".Contains

    let splitLength parts codeLength =
        let min = codeLength / parts
        let remainder = codeLength % parts

        [|
            yield! Array.create (parts - remainder) min
            yield! Array.create remainder (min + 1)
        |]
        |> Array.filter ((<>) 0)

    let generateCode parameters existingCodes (input : string) =
        let encodeWord parameters (input : string) =
            match parameters.PreferConsonants with
            | Some fillupMode ->
                [|
                    yield 0, input.[0]

                    let vowels, notVowels =
                        input.[1..]
                        |> Seq.mapi (fun index c -> index + 1, c)
                        |> Seq.toList
                        |> List.partition (snd >> isVowel)

                    yield! notVowels

                    match fillupMode with
                    | DoNotFillup -> ()
                    | FillFromStart -> yield! vowels
                    | FillFromEnd -> yield! vowels |> List.rev
                |]
                |> Array.truncate parameters.CodeLength
                |> Array.sortBy fst
                |> Array.map snd
                |> String
            | None -> input.Substring(0, Math.Min(input.Length, parameters.CodeLength))

        let parts = input.Split [| ' ' |]

        let split = splitLength parts.Length parameters.CodeLength

        (([], 0), Seq.zip parts split)
        ||> Seq.fold (fun (codes, difference) (part, length) ->
            let length = length + difference
            let code = encodeWord { parameters with CodeLength = length } part

            code :: codes, length - code.Length)
        |> fst
        |> Seq.rev
        |> String.concat ""

    type FillupWithVowelsEnum = DoNotFillup = 0 | FillFromStart = 1 | FillFromEnd = 2

    let toProperFillupMode mode =
        match mode with
        | FillupWithVowelsEnum.DoNotFillup -> DoNotFillup
        | FillupWithVowelsEnum.FillFromStart -> FillFromStart
        | FillupWithVowelsEnum.FillFromEnd -> FillFromEnd
        | _ -> failwith "Unrecognized fillup mode"

    [<Theory>]
    [<InlineData("Sea", 2, "Se")>]
    [<InlineData("Sea", 3, "Sea")>]
    [<InlineData("Sea", 4, "Sea")>]
    [<InlineData("Europe", 4, "Euro")>]
    let ``Code can be generated by simple truncation`` (name, codeLength, expectedResult) =
        // Arrange
        let parameters = { baseParameters with CodeLength = codeLength }

        // Act
        let result = generateCode parameters [] name

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``Code does not contain spaces from name`` () =
        // Arrange
        let name = "A Name"

        let parameters = baseParameters

        let expectedResult = "AN"

        // Act
        let result = generateCode parameters [] name

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Theory>]
    [<InlineData("Europe", 3, FillupWithVowelsEnum.DoNotFillup, "Erp")>]
    [<InlineData("Europe", 4, FillupWithVowelsEnum.DoNotFillup, "Erp")>]
    [<InlineData("A Name", 4, FillupWithVowelsEnum.DoNotFillup, "ANm")>]
    [<InlineData("Europe", 3, FillupWithVowelsEnum.FillFromStart, "Erp")>]
    [<InlineData("Europe", 4, FillupWithVowelsEnum.FillFromStart, "Eurp")>]
    [<InlineData("A Name", 4, FillupWithVowelsEnum.FillFromStart, "ANam")>]
    [<InlineData("Europe", 3, FillupWithVowelsEnum.FillFromEnd, "Erp")>]
    [<InlineData("Europe", 4, FillupWithVowelsEnum.FillFromEnd, "Erpe")>]
    [<InlineData("A Name", 4, FillupWithVowelsEnum.FillFromEnd, "ANme")>]
    let ``Consonants can be preferred as far as available`` (name, codeLength, fillupWithVowels, expectedResult) =
        // Arrange
        let parameters =
            {
                baseParameters with
                    CodeLength = codeLength
                    PreferConsonants =
                        fillupWithVowels |> toProperFillupMode |> Some
            }

        // Act
        let result = generateCode parameters [] name

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Theory>]
    [<InlineData("North America", 5, "NoAme")>]
    [<InlineData("North America", 6, "NorAme")>]
    [<InlineData("Three Part Name", 7, "ThPaNam")>]
    [<InlineData("A B Things", 6, "ABThin")>]
    [<InlineData("A Different Name", 7, "ADifNam")>]
    let ``Multi-word names are converted correctly`` (name, codeLength, expectedResult ) =
        // Arrange
        let parameters = { baseParameters with CodeLength = codeLength }

        // Act
        let result = generateCode parameters [] name

        // Assert
        Assert.StrictEqual (expectedResult, result)

module SplitLengthTests =
    type CodePartLengthsTheoryData() as this =
        inherit TheoryData<int, int, int []>()

        do
            this.Add(5, 2, [| 2; 3 |])
            this.Add(6, 2, [| 3; 3 |])
            this.Add(2, 2, [| 1; 1 |])
            this.Add(1, 2, [| 1 |])
            this.Add(2, 3, [| 1; 1 |])

    [<Theory>]
    [<ClassData(typeof<CodePartLengthsTheoryData>)>]
    let ``Code part lengths are determined correctly`` (codeLength, parts, expectedResult) =
        // Arrange

        // Act
        let result = GenerateCodeTests.splitLength parts codeLength

        // Assert
        Assert.Equal<int []>(expectedResult, result)
