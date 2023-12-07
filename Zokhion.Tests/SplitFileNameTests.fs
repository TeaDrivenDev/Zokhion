namespace TeaDriven.Zokhion.Tests

module SplitFileNameTests =
    open Xunit

    open TeaDriven.Zokhion

    [<Theory>]
    [<InlineData("Aerial view over Uluru at night", "Aerial view over Uluru at night", "", "")>]
    [<InlineData("Aerial view (from above) over Uluru at night", "Aerial view (from above) over Uluru at night", "", "")>]
    [<InlineData("Aerial view over Uluru at night (Uluru)", "Aerial view over Uluru at night", "(Uluru)", "")>]
    [<InlineData("Aerial view over Uluru at night (.Andes.Pacific Ocean.)", "Aerial view over Uluru at night", "(.Andes.Pacific Ocean.)", "")>]
    [<InlineData("Aerial view (from above) over Uluru at night (.Andes.Pacific Ocean.)", "Aerial view (from above) over Uluru at night", "(.Andes.Pacific Ocean.)", "")>]
    [<InlineData("Aerial view over Uluru at night (.Andes.Pacific Ocean.) [.Ax.Bd.]", "Aerial view over Uluru at night", "(.Andes.Pacific Ocean.)", "[.Ax.Bd.]")>]
    [<InlineData("Aerial view over Uluru at night [.Ax.Bd.]", "Aerial view over Uluru at night", "", "[.Ax.Bd.]")>]
    [<InlineData("Aerial view over Uluru at night [NoFeature]", "Aerial view over Uluru at night [NoFeature]", "", "")>]
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
    [<InlineData("Aerial view over Uluru at night [NoFeature]", "Aerial view over Uluru at night [NoFeature]", "", "")>]
    let ``Filename parts are correctly detected omitting separate name part`` (fileName, expectedMain, expectedNames, expectedFeatures) =
        // Arrange
        let fileName = fileName

        let expectedResult = (expectedMain, expectedNames, expectedFeatures)

        // Act
        let result = splitFileName false fileName

        // Assert
        Assert.Equal (expectedResult, result)
