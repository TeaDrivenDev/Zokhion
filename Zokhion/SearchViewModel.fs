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

type SearchViewModelCommand =
    | Directories of (DirectoryInfo option * string)
    | Refresh of FileInfo list

type WithFeatures = HasNoFeatures | HasFeatures | Both

type SearchFilterChange =
    | BaseDirectory of string
    | SelectedDirectory of string
    | SearchValues of string list
    | SearchFromBaseDirectory of bool
    | WithFeatures of WithFeatures

type SearchFilterParameters =
    {
        BaseDirectory: string
        SelectedDirectory: string option
        SearchValues: string list
        WithFeatures: WithFeatures
        SearchFromBaseDirectory: bool
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

type SearchViewModel(commands: IObservable<SearchViewModelCommand>) as this =
    inherit ReactiveObject()

    let mutable baseDirectory = ""
    let selectedDirectory = new BehaviorSubject<DirectoryInfo option>(None)

    let searchString = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let searchFromBaseDirectory = new ReactiveProperty<_>(true)
    let mutable canToggleSearchFromBaseDirectory =
        Unchecked.defaultof<ReadOnlyReactiveProperty<bool>>
    let isActive = new ReactiveProperty<_>(true)
    let files = ObservableCollection()
    let mutable header = Unchecked.defaultof<ReadOnlyReactiveProperty<string>>
    let selectedFile = new ReactiveProperty<FileInfo>()
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
                                    then m.Groups.["size"].Value |> Int32.Parse |> (*) (1024 * 1024) |> SmallerThan
                                    else
                                        let m = largerThanRegex.Match part

                                        if m.Success
                                        then m.Groups.["size"].Value |> Int32.Parse |> (*) (1024 * 1024) |> LargerThan
                                        else Contains [ toUpper part ])
                                |> asSnd (None, [], None)
                                ||> List.fold (fun (smaller, contains, larger) current ->
                                    match current with
                                    | SmallerThan smaller -> Some smaller, contains, larger
                                    | Contains parts -> smaller, parts @ contains, larger
                                    | LargerThan larger -> smaller, contains, Some larger)

                            [
                                yield
                                    smaller
                                    |> Option.map (fun smaller -> fun (fi: FileInfo) -> fi.Length < int64 smaller)

                                yield
                                    larger
                                    |> Option.map (fun larger -> fun (fi: FileInfo) -> fi.Length > int64 larger)

                                yield
                                    match contains with
                                    | [] -> None
                                    | contains ->
                                        (fun (fi: FileInfo) ->
                                            fi.Name
                                            |> Path.GetFileNameWithoutExtension
                                            |> toUpper
                                            |> (fun s -> [ s; s.Replace("_", " ") ])
                                            |> Seq.exists (containsAll contains))
                                            |> Some
                            ]

                    let checkHasFeatures (fi: FileInfo) =
                        fi.FullName
                        |> Path.GetFileNameWithoutExtension
                        |> hasFeaturesRegex.IsMatch

                    yield
                        match searchFilterParameters.WithFeatures with
                        | HasNoFeatures -> Some (checkHasFeatures >> not)
                        | HasFeatures -> Some checkHasFeatures
                        | Both -> None
                ]
                |> List.choose id

            fun (fi: FileInfo) -> filters |> Seq.forall (fun filter -> filter fi)

        let filter =
            let checkFilter =
                fun (fi: FileInfo) ->
                    (toUpper fi.FullName).StartsWith(searchDirectory |> Option.defaultValue "" |> toUpper)
                    && searchFilters fi

            fun filterMode (fi: FileInfo) ->
                match filterMode with
                | Search -> searchFilters fi
                | CheckAffected -> checkFilter fi

        {
            Filter = filter
            SearchDirectory = searchDirectory
        }

    let getFiles filter =
        filter.SearchDirectory
        |> Option.filter (String.IsNullOrWhiteSpace >> not <&&> Directory.Exists)
        |> Option.map (fun dir ->
            Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
            |> Seq.map FileInfo
            |> Seq.filter (filter.Filter Search))

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

        clearSearchStringCommand <- ReactiveCommand.Create(fun () -> searchString.Value <- "")

        refreshCommand <- ReactiveCommand.Create<_>(fun () -> Refresh [])

        isUpdating <-
            [
                isUpdatingNotifier |> Observable.filter not

                isUpdatingNotifier |> Observable.throttle (TimeSpan.FromSeconds 1.5)
            ]
            |> Observable.mergeSeq
            |> toReadOnlyReactiveProperty

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
                    | Refresh _ -> [])

                searchString
                |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
                |> Observable.map
                    (toUpper
                     >> split [| "&&" |]
                     >> Array.map trim
                     >> Array.toList
                     >> SearchValues
                     >> List.singleton)
                |> Observable.distinctUntilChanged

                searchFromBaseDirectory |> Observable.map (SearchFromBaseDirectory >> List.singleton)

                Observable.combineLatest filterHasNoFeatures filterHasFeatures
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
                }
                (fun parameters changes ->
                    (parameters, changes)
                    ||> List.fold (fun current change ->
                        match change with
                        | BaseDirectory ``base`` -> { current with BaseDirectory = ``base`` }
                        | SelectedDirectory dir -> { current with SelectedDirectory = Some dir }
                        | SearchValues searchValues -> { current with SearchValues = searchValues }
                        | WithFeatures withFeatures -> { current with WithFeatures = withFeatures }
                        | SearchFromBaseDirectory fromBase ->
                            { current with SearchFromBaseDirectory = fromBase }))
            |> Observable.map createFilter
            |> toReadOnlyReactiveProperty

        filter
        |> Observable.iter (fun _ -> isUpdatingNotifier.TurnOn())
        |> Observable.observeOn ThreadPoolScheduler.Instance
        |> Observable.choose getFiles
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe (fun newFiles ->
            (newFiles, (files |> Seq.toList))
            ||> fullOuterJoin (fun fi -> fi.FullName) (fun (fi: FileInfo) -> fi.FullName)
            |> Seq.iter (function
                | LeftOnly fi -> files.Remove fi |> ignore
                | RightOnly fi -> files.Add fi
                | JoinMatch (old, ``new``) ->
                    if (``new``.Length, ``new``.LastWriteTimeUtc)
                       <> (old.Length, old.LastWriteTimeUtc)
                    then
                        files.Remove old |> ignore
                        files.Add ``new``)

            isUpdatingNotifier.TurnOff()

            this.RaisePropertyChanged(nameof <@ this.Files @>))
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
                |> fun refresh -> if refresh then refreshSubject.OnNext ())
        |> ignore

        isActive
        |> Observable.filter id
        |> Observable.subscribe (fun _ -> selectedFile.ForceNotify())
        |> ignore

        selectedFileWhenActive <-
            let observable = selectedFile |> Observable.filter (fun _ -> isActive.Value)
            observable.ToReadOnlyReactiveProperty(mode = ReactivePropertyMode.RaiseLatestValueOnSubscribe)

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
