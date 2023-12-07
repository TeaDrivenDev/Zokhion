namespace TeaDriven.Zokhion.Tests

module SettingsTests =
    open Xunit

    open TeaDriven.Zokhion.Settings

    [<Fact>]
    let ``A feature hierarchy is serialized in the correct format`` () =
        // Arrange
        let values =
            [
                {
                    Name = "Scene"
                    Code = "S"
                    Include = None
                    Instances =
                        [
                            { Name = "Sunset"; Code = "Ss" }
                            { Name = "Pre-dawn"; Code = "Pd" }
                        ]
                }

                {
                    Name = "Type of shot"
                    Code = "T"
                    Include = Some "XAb"
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
                "T|Type of shot > XAb"
                "	TAd|Aerial, distant"
                "	TAo|Aerial, overhead"
                "	TGd|Ground level, distant"
            ]

        // Act
        let result: string list = serializeFeatures values

        // Assert
        Assert.StrictEqual (expectedResult, result)

    [<Fact>]
    let ``A feature list without instances is correctly deserialized`` () =
        // Arrange
        let values =
            [
                "S|Scene"
                "T|Type of shot > XAb"
            ]

        let expectedResult =
            [
                { Name = "Scene"; Code = "S"; Include = None; Instances = [] }
                { Name = "Type of shot"; Code = "T"; Include = Some "XAb"; Instances = [] }
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
                "T|Type of shot > XAb"
                "	TAd|Aerial, distant"
                "	TAo|Aerial, overhead"
                "	TGd|Ground level, distant"
            ]

        let expectedResult =
            [
                {
                    Name = "Scene"
                    Code = "S"
                    Include = None
                    Instances =
                        [
                            { Name = "Sunset"; Code = "Ss" }
                            { Name = "Pre-dawn"; Code = "Pd" }
                        ]
                }

                {
                    Name = "Type of shot"
                    Code = "T"
                    Include = Some "XAb"
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
