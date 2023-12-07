namespace TeaDriven.Zokhion

open FileSystem

module GroupingTypes =
    type FileInstance =
        {
            Group: string
            NumberOfInstances: int
            FileInfo: FileInfoCopy
        }

    type GroupCategory =
        | NoGrouping
        | ByIndividualNames
        | ByCoOccurringNames
        | ByDirectory
        | ByFeature of Feature

[<RequireQualifiedAccess>]
module Grouping =
    open GroupingTypes

    let singleInstanceWithGroup group fileInfo =
        { Group = group; NumberOfInstances = 1; FileInfo = fileInfo }

    let multiplexByGroups fileInfo groups =
        match groups with
        | [] -> [ singleInstanceWithGroup "" fileInfo ]
        | groups ->
            groups
            |> List.map
                (fun group ->
                    {
                        Group = group
                        NumberOfInstances = groups.Length
                        FileInfo = fileInfo
                    })

    let extractNames fileName =
        let _, names, _=
            Path.getFileNameWithoutExtension fileName
            |> Renaming.splitFileName true

        let m = Renaming.markedNamesPartRegex.Match names

        if m.Success
        then m.Groups.["names"].Value.Split [| '.' |] |> Array.map trim |> Array.toList
        else []

    let groupByCooccurringNames (files: FileInfoCopy list) =
        files
        |> List.collect
            (fun fileInfo ->
                let names =
                    fileInfo.Name
                    |> extractNames
                    |> String.concat ", "

                [ singleInstanceWithGroup names fileInfo ])

    let groupByIndividualNames (files: FileInfoCopy list) =
        files
        |> List.collect
            (fun fileInfo ->
                fileInfo.Name
                |> extractNames
                |> multiplexByGroups fileInfo)

    let groupByDirectory (files: FileInfoCopy list) =
        files
        |> List.collect
            (fun fileInfo ->
                let directory =
                    fileInfo.FullName
                    |> Path.getDirectoryName
                    |> Path.getFileName

                [ singleInstanceWithGroup directory fileInfo ])

    let groupByFeatureInstances feature (files: FileInfoCopy list) =
        let featureInstanceCodes =
            feature.Instances
            |> List.map (fun instance -> feature.Code + instance.Code)
            |> Set.ofList

        files
        |> List.collect
            (fun fileInfo ->
                let _, _, features =
                    Path.getFileNameWithoutExtension fileInfo.Name
                    |> Renaming.splitFileName false

                Renaming.evaluateFeaturesPart features
                |> Option.defaultValue []
                |> Set.ofList
                |> Set.intersect featureInstanceCodes
                |> Set.toList
                |> multiplexByGroups fileInfo)

    let groupFilesByCategory groupCategory (files: FileInfoCopy list) =
        let groupSelector =
            match groupCategory with
            | NoGrouping -> List.map (singleInstanceWithGroup "")
            | ByIndividualNames -> groupByIndividualNames
            | ByCoOccurringNames -> groupByCooccurringNames
            | ByDirectory -> groupByDirectory
            | ByFeature feature -> groupByFeatureInstances feature

        groupSelector files
