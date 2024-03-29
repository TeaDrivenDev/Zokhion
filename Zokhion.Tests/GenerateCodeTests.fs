﻿namespace TeaDriven.Zokhion.Tests

open Xunit

module GenerateCodeTests =
    open System

    type FillupWithVowels = DoNotFillup | FillFromStart | FillFromEnd

    type GenerateCodeParameters =
        {
            CodeLength: int
            PreferConsonants: FillupWithVowels option
            KeepAdditionalCapitalLetters: bool
        }

    let baseParameters =
        {
            CodeLength = 2
            PreferConsonants = None
            KeepAdditionalCapitalLetters = true
        }

    let isVowel (c: char) = c |> string |> "aeiou".Contains

    let splitLength parts codeLength =
        let min = codeLength / parts
        let remainder = codeLength % parts

        [|
            yield! Array.create (parts - remainder) min
            yield! Array.create remainder (min + 1)
        |]
        |> Array.filter ((<>) 0)

    let generateCode parameters existingCodes (input: string) =
        let encodeWord parameters (input: string) =
            match parameters.PreferConsonants with
            | Some fillupMode ->
                [|
                    0, input.[0]

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
        ||> Seq.fold
            (fun (codes, difference) (part, length) ->
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
    open Xunit

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
