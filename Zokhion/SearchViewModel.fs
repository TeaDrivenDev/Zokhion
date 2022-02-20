namespace TeaDriven.Zokhion.ViewModels

open System
open System.Collections.ObjectModel
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Text.RegularExpressions

open FSharp.Control.Reactive

open Reactive.Bindings
open Reactive.Bindings.Notifiers

open ReactiveUI

open TeaDriven.Zokhion
open TeaDriven.Zokhion.FileSystem

[<AllowNullLiteral>]
type FileViewModel(fileInstance: FileInstance) =
    let name = fileInstance.FileInfo.Name
    let fullName = fileInstance.FileInfo.FullName
    let directoryName = fileInstance.FileInfo.DirectoryName
    let lastWriteTime = fileInstance.FileInfo.LastWriteTime
    let length = fileInstance.FileInfo.Length
    let creationTime = fileInstance.FileInfo.CreationTime

    member __.Group = fileInstance.Group
    member __.FileInfo = fileInstance.FileInfo
    member __.Name = name
    member __.FullName = fullName
    member __.DirectoryName = directoryName
    member __.LastWriteTime = lastWriteTime
    member __.Length = length
    member __.CreationTime = creationTime
    member __.NumberOfInstances = fileInstance.NumberOfInstances

type SearchViewModelCommand =
    | Directories of (DirectoryInfo option * string)
    | Refresh of FileInfo list
    | InitialSearchString of string
    | EnableTab

type WithFeatures = HasNoFeatures | HasFeatures | Both

type SearchFilterChange =
    | BaseDirectory of string
    | SelectedDirectory of string
    | SearchValues of string list
    | SearchFromBaseDirectory of bool
    | WithFeatures of WithFeatures
    | Enable

type SearchFilterParameters =
    {
        BaseDirectory: string
        SelectedDirectory: string option
        SearchValues: string list
        WithFeatures: WithFeatures
        SearchFromBaseDirectory: bool
        IsEnabled: bool
    }

type SearchCriterion =
    | Contains of string list
    | SmallerThan of int
    | LargerThan of int

type FilterMode = Search | CheckAffected

type SearchFilter =
    {
        Filter: FilterMode -> FileInfo -> bool
        SearchDirectory: string option
    }

type DisplayParameters =
    {
        GroupCategory: GroupCategory
        SearchFilter: SearchFilter
    }

type DisplayParameterChange =
    | GroupCategory of GroupCategory
    | SearchFilter of SearchFilter

type FileViewModelOperation = Keep | Replace | Remove

type SearchViewModel(commands: IObservable<SearchViewModelCommand>) as this =
    inherit ReactiveObject()

    let mutable baseDirectory = ""
    let selectedDirectory = new BehaviorSubject<DirectoryInfo option>(None)

    let isGroupByFeatureValue = new ReactiveProperty<bool>()
    let groupCategory = new ReactiveProperty<GroupCategory>(NoGrouping)

    let searchString = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let searchFromBaseDirectory = new ReactiveProperty<_>(true)
    let mutable canToggleSearchFromBaseDirectory =
        Unchecked.defaultof<ReadOnlyReactiveProperty<bool>>
    let isActive = new ReactiveProperty<_>(true)
    let files = ObservableCollection<FileViewModel>()
    let mutable header = Unchecked.defaultof<ReadOnlyReactiveProperty<string>>
    let selectedFile = new ReactiveProperty<FileViewModel>()
    let mutable selectedFileWhenActive = Unchecked.defaultof<ReadOnlyReactiveProperty<_>>
    let mutable refreshCommand = Unchecked.defaultof<ReactiveCommand<_, _>>
    let mutable clearSearchStringCommand = Unchecked.defaultof<ReactiveCommand>

    let refreshSubject = new System.Reactive.Subjects.Subject<_>()

    let filterHasNoFeatures = new ReactiveProperty<_>(false)
    let filterHasFeatures = new ReactiveProperty<_>(false)

    let isUpdatingNotifier = BooleanNotifier(false)
    let mutable isUpdating = Unchecked.defaultof<ReadOnlyReactiveProperty<_>>

    let mutable filter = Unchecked.defaultof<ReadOnlyReactiveProperty<_>>

    let smallerThanRegex = Regex(@"^<\s*(?<size>\d+)MB$", RegexOptions.Compiled)
    let largerThanRegex = Regex(@"^>\s*(?<size>\d+)MB$", RegexOptions.Compiled)
    let hasFeaturesRegex = Regex(@"^.+\[\..+\.\]$", RegexOptions.Compiled)

    let createFilter searchFilterParameters =
        let searchDirectory =
            if searchFilterParameters.SearchFromBaseDirectory
               && searchFilterParameters.SearchValues <> []
            then Some searchFilterParameters.BaseDirectory
            else searchFilterParameters.SelectedDirectory

        let searchFilters =
            let filters =
                [
                    yield!
                        match searchFilterParameters.SearchValues with
                        | [] -> [ Some (fun _ -> true) ]
                        | _ ->
                            let smaller, contains, larger =
                                searchFilterParameters.SearchValues
                                |> List.map (fun part ->
                                    let m = smallerThanRegex.Match part

                                    if m.Success
                                    then
                                        m.Groups.["size"].Value
                                        |> Int32.Parse
                                        |> (*) (1024 * 1024)
                                        |> SmallerThan
                                    else
                                        let m = largerThanRegex.Match part

                                        if m.Success
                                        then
                                            m.Groups.["size"].Value
                                            |> Int32.Parse
                                            |> (*) (1024 * 1024)
                                            |> LargerThan
                                        else Contains [ toUpper part ])
                                |> asSnd (None, [], None)
                                ||> List.fold (fun (smaller, contains, larger) current ->
                                    match current with
                                    | SmallerThan smaller -> Some smaller, contains, larger
                                    | Contains parts -> smaller, parts @ contains, larger
                                    | LargerThan larger -> smaller, contains, Some larger)

                            [
                                smaller
                                |> Option.map (fun smaller ->
                                    fun (file: FileInfo) -> file.Length < int64 smaller)

                                larger
                                |> Option.map (fun larger ->
                                    fun (file: FileInfo) -> file.Length > int64 larger)

                                match contains with
                                | [] -> None
                                | contains ->
                                    (fun (file: FileInfo) ->
                                        file.Name
                                        |> Path.getFileNameWithoutExtension
                                        |> toUpper
                                        |> (fun s -> [ s; s.Replace("_", " ") ])
                                        |> Seq.exists (containsAll contains))
                                        |> Some
                            ]

                    let checkHasFeatures (file: FileInfo) =
                        file.FullName
                        |> Path.getFileNameWithoutExtension
                        |> hasFeaturesRegex.IsMatch

                    match searchFilterParameters.WithFeatures with
                    | HasNoFeatures -> Some (checkHasFeatures >> not)
                    | HasFeatures -> Some checkHasFeatures
                    | Both -> None
                ]
                |> List.choose id

            fun (file: FileInfo) -> filters |> Seq.forall (fun filter -> filter file)

        let filter =
            let checkFilter =
                fun (file: FileInfo) ->
                    (toUpper file.FullName)
                        .StartsWith(searchDirectory |> Option.defaultValue "" |> toUpper)
                    && searchFilters file

            fun filterMode (file: FileInfo) ->
                match filterMode with
                | Search -> searchFilters file
                | CheckAffected -> checkFilter file

        {
            Filter = filter
            SearchDirectory = searchDirectory
        }

    let getFiles filter =
        filter.SearchDirectory
        |> Option.filter (String.IsNullOrWhiteSpace >> not <&&> Directory.exists)
        |> Option.map (fun dir ->
            Directory.getFiles dir
            |> Seq.map FileInfo
            |> Seq.filter (filter.Filter Search))

    let tryAddFile (files: ObservableCollection<_>) fileInstance =
        try
            FileViewModel fileInstance
            |> Some
        with _ -> None
        |> Option.iter files.Add

    do
        header <-
            searchString
            |> Observable.map (function
                | "" ->
                    selectedDirectory.Value
                    |> Option.map (fun selected -> selected.Name)
                    |> Option.defaultValue " (none) "
                    |> sprintf "<%s>"
                | search -> search)
            |> Observable.startWith [ "Search" ]
            |> toReadOnlyReactiveProperty

        canToggleSearchFromBaseDirectory <-
            selectedDirectory
            |> Observable.map Option.isSome
            |> toReadOnlyReactiveProperty

        clearSearchStringCommand <-
            ReactiveCommand.Create(fun () -> searchString.Value <- "")

        refreshCommand <- ReactiveCommand.Create<_>(fun () -> Refresh [])

        isUpdating <-
            [
                isUpdatingNotifier |> Observable.filter not

                isUpdatingNotifier |> Observable.throttle (TimeSpan.FromSeconds 1.5)
            ]
            |> Observable.mergeSeq
            |> toReadOnlyReactiveProperty

        let splitSearchString =
            toUpper
            >> split [| "&&" |]
            >> Array.map trim
            >> Array.toList
            >> SearchValues

        filter <-
            [
                commands
                |> Observable.filter (fun _ -> isActive.Value)
                |> Observable.map (function
                    | Directories (dir, ``base``) ->
                        [
                            dir |> Option.map (fun di -> SelectedDirectory di.FullName)
                            Some (BaseDirectory ``base``)
                        ]
                        |> List.choose id
                    | Refresh _ -> []
                    | InitialSearchString searchString ->
                        [
                            splitSearchString searchString
                            SearchFromBaseDirectory true
                        ]
                    | EnableTab -> [ Enable ])

                searchString
                |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
                |> Observable.map (splitSearchString >> List.singleton)
                |> Observable.distinctUntilChanged

                searchFromBaseDirectory
                |> Observable.map (SearchFromBaseDirectory >> List.singleton)

                (filterHasNoFeatures, filterHasFeatures)
                ||> Observable.combineLatest
                |> Observable.map (fun flags ->
                    match flags with
                    | true, false -> HasNoFeatures
                    | false, true -> HasFeatures
                    | _ -> Both
                    |> WithFeatures
                    |> List.singleton)

                refreshSubject |> Observable.map (fun () -> [])
            ]
            |> Observable.mergeSeq
            |> Observable.scanInit
                {
                    BaseDirectory = ""
                    SelectedDirectory = None
                    SearchValues = []
                    WithFeatures = Both
                    SearchFromBaseDirectory = searchFromBaseDirectory.Value
                    IsEnabled = false
                }
                (fun parameters changes ->
                    (parameters, changes)
                    ||> List.fold (fun current change ->
                        match change with
                        | BaseDirectory ``base`` ->
                            { current with BaseDirectory = ``base`` }
                        | SelectedDirectory dir ->
                            { current with SelectedDirectory = Some dir }
                        | SearchValues searchValues ->
                            { current with SearchValues = searchValues }
                        | WithFeatures withFeatures ->
                            { current with WithFeatures = withFeatures }
                        | SearchFromBaseDirectory fromBase ->
                            { current with SearchFromBaseDirectory = fromBase }
                        | Enable -> { current with IsEnabled = true }))
            |> Observable.filter (fun filter -> filter.IsEnabled)
            |> Observable.map createFilter
            |> toReadOnlyReactivePropertyWithMode ReactivePropertyMode.None

        [
            groupCategory |> Observable.map GroupCategory

            filter |> Observable.map SearchFilter
        ]
        |> Observable.mergeSeq
        |> Observable.scanInit
            {
                GroupCategory = NoGrouping
                SearchFilter =
                    {
                        Filter = fun mode file -> true
                        SearchDirectory = None
                    }
            }
            (fun parameters (change: DisplayParameterChange) ->
                match change with
                | GroupCategory feature -> { parameters with GroupCategory = feature}
                | SearchFilter filter -> { parameters with SearchFilter = filter})
        |> Observable.iter (fun _ -> isUpdatingNotifier.TurnOn())
        |> Observable.observeOn ThreadPoolScheduler.Instance
        |> Observable.choose (fun parameters ->
            getFiles parameters.SearchFilter
            |> Option.map (Seq.toList >> asSnd parameters.GroupCategory))
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe (fun (groupCategory, newFiles) ->
            let newFiles = Logic.groupFilesByCategory groupCategory newFiles

            (newFiles |> List.map JoinWrapper, Seq.toList files)
            ||> fullOuterJoin
                (fun newFile ->
                    let { Group = group; FileInfo = file } = newFile.Value
                    group, file.FullName)
                (fun viewModel -> viewModel.Group, viewModel.FileInfo.FullName)
            |> Seq.iter (function
                | LeftOnly vm -> files.Remove vm |> ignore
                | RightOnly (JoinWrapped fileInstance) ->
                    fileInstance |> tryAddFile files
                | JoinMatch (oldViewModel, JoinWrapped newFileInstance) ->
                    let inline sizeAndDate (file: FileInfo) =
                        file.Length, file.LastWriteTimeUtc

                    try
                        if (sizeAndDate newFileInstance.FileInfo, newFileInstance.NumberOfInstances)
                            <> (sizeAndDate oldViewModel.FileInfo, oldViewModel.NumberOfInstances)
                        then Replace
                        else Keep
                    with
                    | :? FileNotFoundException as ex -> Remove
                    |> function
                        | Keep -> ()
                        | Remove -> files.Remove oldViewModel |> ignore
                        | Replace ->
                            files.Remove oldViewModel |> ignore

                            newFileInstance |> tryAddFile files)

            isUpdatingNotifier.TurnOff()

            this.RaisePropertyChanged(nameof this.Files))
        |> ignore

        [ refreshCommand.AsObservable(); commands ]
        |> Observable.mergeSeq
        |> Observable.subscribe (function
            | Directories (selected, ``base``) ->
                if isActive.Value
                then
                    selectedDirectory.OnNext selected
                    baseDirectory <- ``base``
                    searchFromBaseDirectory.Value <- Option.isNone selected

                    searchString.Value <- ""
            | Refresh files ->
                match files with
                | [] -> isActive.Value
                | _ -> files |> List.exists (filter.Value.Filter CheckAffected)
                |> fun refresh -> if refresh then refreshSubject.OnNext ()
            | InitialSearchString _
            | EnableTab -> ())
        |> ignore

        isActive
        |> Observable.filter id
        |> Observable.subscribe (fun _ -> selectedFile.ForceNotify())
        |> ignore

        selectedFileWhenActive <-
            selectedFile
            |> Observable.filter (fun _ -> isActive.Value)
            |> Observable.map (fun vm -> if isNull vm then null else vm.FileInfo)
            |> toReadOnlyReactivePropertyWithMode ReactivePropertyMode.RaiseLatestValueOnSubscribe

    member __.IsGroupByFeatureValue = isGroupByFeatureValue
    member __.FeatureToGroupBy = groupCategory
    member __.SearchString = searchString
    member __.SearchFromBaseDirectory = searchFromBaseDirectory
    member __.CanToggleSearchFromBaseDirectory = canToggleSearchFromBaseDirectory
    member __.IsActive = isActive
    member __.Files = files
    member __.Header = header
    member __.SelectedFile = selectedFile
    member __.SelectedFileWhenActive = selectedFileWhenActive
    member __.RefreshCommand = refreshCommand
    member __.ClearSearchStringCommand = clearSearchStringCommand
    member __.IsUpdating = isUpdating
    member __.FilterHasNoFeatures = filterHasNoFeatures
    member __.FilterHasFeatures = filterHasFeatures
