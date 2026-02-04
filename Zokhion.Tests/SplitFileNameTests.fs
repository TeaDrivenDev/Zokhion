namespace TeaDriven.Zokhion.Tests

module SplitFileNameTests =
    open Xunit

    open TeaDriven.Zokhion.Core

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

        // Act
        let actualMain, actualNames, actualFeatures = Renaming.splitFileName true fileName

        // Assert
        Assert.Multiple(
            (fun _ -> Assert.Equal(expectedMain, actualMain)),
            (fun _ -> Assert.Equal(expectedNames, actualNames)),
            (fun _ -> Assert.Equal(expectedFeatures, actualFeatures)))

    [<Theory>]
    [<InlineData("Aerial view over Uluru at night", "Aerial view over Uluru at night", "", "")>]
    [<InlineData("Aerial view (from above) over Uluru at night", "Aerial view (from above) over Uluru at night", "", "")>]
    [<InlineData("Aerial view over Uluru at night (Uluru)", "Aerial view over Uluru at night (Uluru)", "", "")>]
    [<InlineData("Aerial view over Uluru at night (.Andes.Pacific Ocean.)", "Aerial view over Uluru at night", "(.Andes.Pacific Ocean.)", "")>]
    [<InlineData("Aerial view (from above) over Uluru at night (.Andes.Pacific Ocean.)", "Aerial view (from above) over Uluru at night", "(.Andes.Pacific Ocean.)", "")>]
    [<InlineData("Aerial view over Uluru at night (.Andes.Pacific Ocean.) [.Ax.Bd.]", "Aerial view over Uluru at night", "(.Andes.Pacific Ocean.)", "[.Ax.Bd.]")>]
    [<InlineData("Aerial view over Uluru at night [.Ax.Bd.]", "Aerial view over Uluru at night", "", "[.Ax.Bd.]")>]
    [<InlineData("Aerial view over Uluru at night [NoFeature]", "Aerial view over Uluru at night [NoFeature]", "", "")>]
    let ``Filename parts are correctly detected when omitting unmarked names part`` (fileName, expectedMain, expectedNames, expectedFeatures) =
        // Arrange
        let fileName = fileName

        // Act
        let actualMain, actualNames, actualFeatures = Renaming.splitFileName false fileName

        // Assert
        Assert.Multiple(
            (fun _ -> Assert.Equal(expectedMain, actualMain)),
            (fun _ -> Assert.Equal(expectedNames, actualNames)),
            (fun _ -> Assert.Equal(expectedFeatures, actualFeatures)))
