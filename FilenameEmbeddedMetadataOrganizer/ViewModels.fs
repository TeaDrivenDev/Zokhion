namespace FilenameEmbeddedMetadataOrganizer.ViewModels

open System
open System.Collections.ObjectModel
open System.Diagnostics
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Windows
open System.Windows.Input

open FSharp.Control.Reactive

open ReactiveUI

open Reactive.Bindings

open FilenameEmbeddedMetadataOrganizer

type ReactiveCommand = ReactiveUI.ReactiveCommand

[<AutoOpen>]
module Utility =
    open System.Linq.Expressions
    open FSharp.Quotations

    // see https://stackoverflow.com/a/48311816/236507
    let nameof (q:Expr<_>) =
        match q with
        | Patterns.Let(_, _, DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _))) -> mi.Name
        | Patterns.PropertyGet(_, mi, _) -> mi.Name
        | DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _)) -> mi.Name
        | _ -> failwith "Unexpected format"

    let any<'R> : 'R = failwith "!"

    // From http://stackoverflow.com/questions/2682475/converting-f-quotations-into-linq-expressions
    /// Converts a F# Expression to a LINQ Lambda
    let toLambda (exp:Expr) =
        let linq = FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.QuotationToExpression exp :?> MethodCallExpression
        linq.Arguments.[0] :?> LambdaExpression

    /// Converts a Lambda quotation into a Linq Lamba Expression with 1 parameter
    let toLinq (exp : Expr<'a -> 'b>) =
        let lambda = toLambda exp
        Expression.Lambda<Func<'a, 'b>>(lambda.Body, lambda.Parameters)

    let withLatestFrom (observable1 : IObservable<_>) (observable2 : IObservable<_>) =
        observable2.WithLatestFrom(observable1, fun a b -> a, b)

    let xwhen (observable2 : IObservable<_>) (observable1 : IObservable<_>) =
        observable1 |> withLatestFrom observable2 |> Observable.filter snd

    let contains part (s : string) = s.Contains part

    let toReadOnlyReactiveProperty (observable : IObservable<_>) =
        observable.ToReadOnlyReactiveProperty()

[<AllowNullLiteral>]
type FeatureViewModel(feature : Feature) as this =
    inherit ReactiveObject()

    let instances = ReactiveList<FeatureInstanceViewModel>()

    do
        match this with
        | :? FeatureInstanceViewModel -> ()
        | _ ->
            feature.Instances
            |> List.map (fun instance -> FeatureInstanceViewModel(feature, instance))
            |> instances.AddRange

    member __.FeatureName = feature.Name

    member __.FeatureCode = feature.Code

    member __.Instances = instances

    member __.Feature =
        { feature with Instances = instances |> Seq.map (fun vm -> vm.Instance) |> Seq.toList }

    member val IsExpanded = new ReactiveProperty<_>(false)

    member __.ResetExpanded () =
        __.IsExpanded.Value <- __.Instances |> Seq.exists (fun vm -> vm.IsSelected)

and [<AllowNullLiteral>]
    FeatureInstanceViewModel(feature : Feature, instance : FeatureInstance) =
    inherit FeatureViewModel(feature)

    let mutable isSelected = false

    member __.InstanceName = instance.Name

    member __.InstanceCode = instance.Code

    member __.CompositeInstanceCode = feature.Code + instance.Code

    member __.IsSelected
        with get () = isSelected
        and set value = __.RaiseAndSetIfChanged(&isSelected, value, nameof <@ __.IsSelected @>) |> ignore

    member __.Instance = instance

[<AllowNullLiteral>]
type NameViewModel(name : string, isSelected : bool, isNew : bool) =
    inherit ReactiveObject()

    let mutable xIsSelected = isSelected

    member val Name = new ReactiveProperty<_>(name)

    member __.IsSelected
        with get () = xIsSelected
        and set value = __.RaiseAndSetIfChanged(&xIsSelected, value, nameof <@ __.IsSelected @>) |> ignore

    member val IsNew : ReactiveProperty<bool> = new ReactiveProperty<_>(isNew)

    member __.ClearNewFlagCommand = ReactiveCommand.Create(fun () -> __.IsNew.Value <- false)

type SearchViewModelCommand =
    | SelectedDirectory of (DirectoryInfo * string)
    | ResetSearch

type SearchViewModel(commands : IObservable<SearchViewModelCommand>) =

    let mutable baseDirectory = ""
    let mutable selectedDirectory = Unchecked.defaultof<DirectoryInfo>

    let searchString = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let searchFromBaseDirectory = new ReactiveProperty<_>(false)
    let isActive = new ReactiveProperty<_>(true)
    let files = ObservableCollection()
    let mutable header = Unchecked.defaultof<ReadOnlyReactiveProperty<string>>
    let selectedFile = new ReactiveProperty<FileInfo>()

    let getFiles searchString fromBaseDirectory =
        if not <| isNull selectedDirectory && selectedDirectory.Exists
        then
            files.Clear()

            let filter searchString =
                if String.IsNullOrWhiteSpace searchString
                then (fun _ -> true)
                else
                    (fun (fi : FileInfo) ->
                        fi.Name
                        |> Path.GetFileNameWithoutExtension
                        |> toUpper
                        |> contains (toUpper searchString))

            let directory =
                if fromBaseDirectory && not <| String.IsNullOrWhiteSpace searchString
                then baseDirectory
                else selectedDirectory.FullName

            Directory.GetFiles(directory, "*", SearchOption.AllDirectories)
            |> Seq.map FileInfo
            |> Seq.filter (filter searchString)
            |> Seq.iter files.Add

    do
        header <-
            searchString
            |> Observable.map (function
                | "" ->
                    if isNull selectedDirectory
                    then ""
                    else sprintf "<%s>" selectedDirectory.Name
                | search -> search)
            |> Observable.startWith [ "Search" ]
            |> toReadOnlyReactiveProperty

        searchString
        |> Observable.combineLatest searchFromBaseDirectory
        |> Observable.subscribe (fun (fromBaseDirectory, searchString) ->
            getFiles searchString fromBaseDirectory)
        |> ignore

        commands
        |> Observable.subscribe (fun command ->
            if isActive.Value
            then
                match command with
                | SelectedDirectory (selected, ``base``) ->
                    selectedDirectory <- selected
                    baseDirectory <- ``base``
                    searchFromBaseDirectory.Value <- false
                | ResetSearch -> ()

                searchString.Value <- "")
        |> ignore

    member __.SearchString = searchString
    member __.SearchFromBaseDirectory = searchFromBaseDirectory
    member __.IsActive = isActive
    member __.Files = files
    member __.Header = header
    member __.SelectedFile = selectedFile

type MainWindowViewModel() as this =
    inherit ReactiveObject()

    let baseDirectory = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let sourceDirectoryPrefixes = new ReactiveProperty<_>("", ReactivePropertyMode.RaiseLatestValueOnSubscribe)
    let selectedDirectory = new ReactiveProperty<_>(Unchecked.defaultof<DirectoryInfo>, ReactivePropertyMode.None)
    let directories = ObservableCollection()

    let isSearchEnabled =
        new ReactiveProperty<bool>(selectedDirectory |> Observable.map (isNull >> not))

    let searchCommands = new ReplaySubject<SearchViewModelCommand>(1)
    let searches = ObservableCollection<SearchViewModel>()
    let activeSearchTab = new ReactiveProperty<SearchViewModel>()
    let selectedFilesSubject = new Subject<IObservable<FileInfo>>()

    let mutable selectedFile = Unchecked.defaultof<ReadOnlyReactiveProperty<FileInfo>>

    let originalFileName = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let newFileName = new ReactiveProperty<_>("", ReactivePropertyMode.None)

    let treatParenthesizedPartAsNames = new ReactiveProperty<_>(true)
    let fixupNamesInMainPart = new ReactiveProperty<_>(false)
    let replaceUnderscores = new ReactiveProperty<_>(true)
    let detectNamesInMainAndNamesParts = new ReactiveProperty<_>(false)
    let recapitalizeNames = new ReactiveProperty<_>(false)

    let mutable openCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable openExplorerCommand = Unchecked.defaultof<ReactiveCommand>

    let selectedDestinationDirectory =
        new ReactiveProperty<_>(Unchecked.defaultof<DirectoryInfo>, ReactivePropertyMode.None)
    let destinationDirectoryPrefixes = new ReactiveProperty<_>("")
    let destinationDirectories = ObservableCollection()

    let newNameToAdd = new ReactiveProperty<_>("")
    let mutable addNameCommand = Unchecked.defaultof<ReactiveCommand>
    let names = ReactiveList([], 0.5, DispatcherScheduler(Application.Current.Dispatcher), ChangeTrackingEnabled = true)
    let mutable resetNameSelectionCommand = Unchecked.defaultof<ReactiveCommand>

    let addFeatureRoot = new ReactiveProperty<_>(false)
    let featureToAdd = new ReactiveProperty<_>()
    let featureCodeToAdd = new ReactiveProperty<_>()
    let mutable addFeatureCommand = Unchecked.defaultof<ReactiveCommand>
    let selectedFeature =
        new ReactiveProperty<_>(Unchecked.defaultof<FeatureViewModel>, ReactivePropertyMode.None)
    let features = ReactiveList()
    let featureInstances = ReactiveList(ChangeTrackingEnabled = true)

    let resultingFilePath = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let mutable applyCommand = Unchecked.defaultof<ReactiveCommand>

    let updateDestinationDirectories (prefixes : string) (currentFilePath : string) =
        let startsWithAny parts (s : string) =
            parts |> Seq.toList |> List.exists (string >> s.StartsWith)

        let currentFileDirectory = Path.GetDirectoryName currentFilePath

        destinationDirectories.Clear()

        let filter =
            if String.IsNullOrWhiteSpace prefixes
            then fun _ -> true
            else startsWithAny prefixes

        currentFileDirectory ::
        (Directory.GetDirectories this.BaseDirectory.Value
        |> Array.filter (Path.GetFileName >> filter)
        |> Array.sort
        |> Array.toList)
        |> List.distinct
        |> List.map DirectoryInfo
        |> List.iter destinationDirectories.Add

        this.SelectedDestinationDirectory.Value <-
            destinationDirectories
            |> Seq.find (fun (d : DirectoryInfo) -> d.FullName = currentFileDirectory)

    let updateNamesList detectedNames =
        (detectedNames, this.Names)
        ||> fullOuterJoin toUpper (fun vm -> vm.Name.Value |> toUpper)
        |> Seq.iter (function
            | LeftOnly vm -> vm.IsSelected <- false
            | RightOnly name ->
                NameViewModel(name, true, true)
                |> names.Add
            | JoinMatch (vm, name) ->
                vm.Name.Value <- name
                vm.IsSelected <- true)

    let updateSelectedFeatures isInitial selectedFeatures =
        (selectedFeatures, featureInstances)
        ||> fullOuterJoin id (fun (vm : FeatureInstanceViewModel) -> vm.CompositeInstanceCode)
        |> Seq.iter (fun result ->
            match result with
            | LeftOnly vm -> vm.IsSelected <- false
            | RightOnly _ -> ()
            | JoinMatch (vm, _) -> vm.IsSelected <- true)

        if isInitial
        then
            let anyFeaturesSelected = featureInstances |> Seq.exists (fun vm -> vm.IsSelected)

            features
            |> Seq.iter (fun (vm : FeatureViewModel) ->
                if anyFeaturesSelected
                then vm.ResetExpanded()
                else vm.IsExpanded.Value <- true)

    let getAllNames () =
        this.Names
        |> Seq.filter (fun vm -> not vm.IsNew.Value)
        |> Seq.map (fun vm -> vm.Name.Value)
        |> Seq.toList

    let updateNewName originalFileName parameters =
        let result = rename parameters originalFileName
        this.NewFileName.Value <- result.NewFileName

        result.DetectedNames
        |> updateNamesList

        result.DetectedFeatures
        |> updateSelectedFeatures (parameters.SelectedFeatures |> Option.isNone)

    let updateResultingFilePath () =
        if not <| isNull this.SelectedDestinationDirectory.Value
        then
            this.SelectedFile.Value
            |> Option.ofObj
            |> Option.iter (fun selectedFile ->
                this.ResultingFilePath.Value <-
                    Path.Combine(this.SelectedDestinationDirectory.Value.FullName,
                                 this.NewFileName.Value + Path.GetExtension(selectedFile.Name)))

    let saveSettings baseDirectory =
        if Directory.Exists baseDirectory
        then
            {
                SourceDirectoryPrefixes = this.SourceDirectoryPrefixes.Value
                DestinationDirectoryPrefixes = this.DestinationDirectoryPrefixes.Value
                Names =
                    this.Names
                    |> Seq.filter (fun vm -> not vm.IsNew.Value)
                    |> Seq.map (fun vm -> vm.Name.Value)
                    |> Seq.distinct
                    |> Seq.sort
                    |> Seq.toList
                Features =
                    this.Features
                    |> Seq.map (fun vm -> vm.Feature)
                    |> Seq.toList
            }
            |> Settings.saveSettings baseDirectory

    let loadSettings baseDirectory =
        let settings = Settings.loadSettings baseDirectory

        this.SourceDirectoryPrefixes.Value <- settings.SourceDirectoryPrefixes
        this.DestinationDirectoryPrefixes.Value <- settings.DestinationDirectoryPrefixes

        names.Clear()

        settings.Names
        |> Seq.iter (fun name -> NameViewModel(name, false, false) |> this.Names.Add)

        features.Clear()
        featureInstances.Clear()

        settings.Features
        |> List.iter (FeatureViewModel >> this.Features.Add)

        this.Features
        |> Seq.collect (fun vm -> vm.Instances)
        |> this.FeatureInstances.AddRange

    let createSearchTab () =
        let search = SearchViewModel(searchCommands.AsObservable())

        selectedFilesSubject.OnNext search.SelectedFile
        this.ActiveSearchTab.Value <- search
        search

    do
        RxApp.MainThreadScheduler <- DispatcherScheduler(Application.Current.Dispatcher)

        selectedFile <-
            selectedFilesSubject
            |> Observable.flatmap id
            |> toReadOnlyReactiveProperty

        openCommand <-
            ReactiveCommand.Create(
                (fun (fi : FileInfo) -> Process.Start fi.FullName |> ignore),
                this.SelectedFile |> Observable.map (fun fi -> not <| isNull fi && fi.Exists))

        openExplorerCommand <-
            ReactiveCommand.Create(fun (fi: FileInfo) ->
                if not <| isNull fi
                then
                    fi.FullName
                    |> sprintf "/select, \"%s\""
                    |> Some
                elif not <| isNull this.SelectedDirectory.Value
                then
                    this.SelectedDirectory.Value.FullName
                    |> sprintf "\"%s\""
                    |> Some
                else None
                |> Option.iter (asSnd "explorer.exe" >> Process.Start >> ignore))

        addNameCommand <-
            ReactiveCommand.Create(fun name ->
                if not <| String.IsNullOrWhiteSpace name
                then
                    NameViewModel(name, false, false) |> this.Names.Add)

        resetNameSelectionCommand <- ReactiveCommand.Create(fun () -> ignore ())

        addFeatureCommand <-
            ReactiveCommand.Create(fun () ->
                if not <| String.IsNullOrWhiteSpace this.FeatureToAdd.Value
                    && not <| String.IsNullOrWhiteSpace this.FeatureCodeToAdd.Value
                then
                    if this.AddFeatureRoot.Value
                    then
                        FeatureViewModel({ Name = this.FeatureToAdd.Value; Code = this.FeatureCodeToAdd.Value; Instances = [] })
                        |> features.Add
                    else
                        match this.SelectedFeature.Value with
                        | :? FeatureInstanceViewModel -> ()
                        | :? FeatureViewModel as feature ->
                            let instance =
                                FeatureInstanceViewModel(feature.Feature, { Name = this.FeatureToAdd.Value; Code = this.FeatureCodeToAdd.Value })

                            feature.Instances.Add instance
                            featureInstances.Add instance
                        | _ -> ()

                    this.FeatureToAdd.Value <- ""
                    this.FeatureCodeToAdd.Value <- "")

        applyCommand <-
            ReactiveCommand.Create(
                (fun () -> File.Move(this.SelectedFile.Value.FullName, this.ResultingFilePath.Value)),
                this.SelectedFile |> Observable.map (fun fi -> not <| isNull fi && fi.Exists))

        this.SelectedDirectory
        |> Observable.map (fun dir -> SelectedDirectory (dir, this.BaseDirectory.Value))
        |> Observable.subscribeObserver searchCommands
        |> ignore

        this.ActiveSearchTab
        |> Observable.subscribe (fun tab ->
            searches
            |> Seq.iter (fun vm -> vm.IsActive.Value <- (tab = vm)))
        |> ignore

        this.FeatureInstances.ItemChanged
        |> Observable.filter (fun change -> change.PropertyName = nameof <@ any<FeatureInstanceViewModel>.IsSelected @>)
        |> Observable.subscribe (fun _ -> this.RaisePropertyChanged(nameof <@ this.SelectedFeatureInstances @>))
        |> ignore

        this.BaseDirectory
        |> Observable.throttle (TimeSpan.FromMilliseconds 500.)
        |> Observable.filter Directory.Exists
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe loadSettings
        |> ignore

        this.SourceDirectoryPrefixes
        |> Observable.combineLatest
            (this.BaseDirectory
             |> Observable.filter Directory.Exists)
        |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromSeconds 1.)
        |> Observable.subscribe (fun (dir, prefixes) ->
            directories.Clear()

            Directory.GetDirectories dir
            |> Seq.map DirectoryInfo
            |> Seq.filter (fun di ->
                match prefixes with
                | "" -> true
                | _ -> prefixes |> Seq.exists (string >> di.Name.StartsWith))
            |> Seq.sortBy (fun di -> di.Name)
            |> Seq.iter directories.Add)
        |> ignore

        createSearchTab () |> searches.Add

        let existingSelectedFile = this.SelectedFile |> Observable.filter (isNull >> not)

        existingSelectedFile
        |> Observable.map (fun fi -> fi.FullName)
        |> Observable.combineLatest
            (this.DestinationDirectoryPrefixes
             |> Observable.throttle (TimeSpan.FromMilliseconds 500.)
             |> Observable.observeOn RxApp.MainThreadScheduler)
        |> Observable.subscribe (uncurry updateDestinationDirectories)
        |> ignore

        this.SelectedFile
        |> Observable.subscribe (fun fi ->
            if not <| isNull fi
            then
                this.Names
                |> Seq.filter (fun vm -> vm.IsNew.Value)
                |> Seq.toList
                |> List.iter (this.Names.Remove >> ignore)

                this.OriginalFileName.Value <-
                    string fi.Name |> Path.GetFileNameWithoutExtension)
        |> ignore

        let gate = new BehaviorSubject<bool>(true)

        [
            this.TreatParenthesizedPartAsNames |> Observable.map TreatParenthesizedPartAsNames
            this.FixupNamesInMainPart |> Observable.map FixupNamesInMainPart
            this.ReplaceUnderscores |> Observable.map ReplaceUnderscores
            this.DetectNamesInMainAndNamesParts |> Observable.map DetectNamesInMainAndNamesParts
            this.RecapitalizeNames |> Observable.map RecapitalizeNames

            this.Names.ItemChanged
            |> Observable.filter (fun change ->
                change.PropertyName = nameof <@ any<NameViewModel>.IsSelected @>)
            |> xwhen gate
            |> Observable.map (fun _ ->
                this.Names
                |> Seq.filter (fun n -> n.IsSelected)
                |> Seq.map (fun n -> n.Name.Value)
                |> Seq.toList
                |> Some
                |> SelectedNames)

            this.ResetNameSelectionCommand.IsExecuting
            |> Observable.distinctUntilChanged
            |> Observable.filter id
            |> Observable.map (fun _ -> SelectedNames None)

            this.OriginalFileName |> Observable.map (fun _ -> ResetSelections)

            this.FeatureInstances.ItemChanged
            |> Observable.filter (fun change ->
                change.PropertyName = nameof <@ any<FeatureInstanceViewModel>.IsSelected @>)
            |> xwhen gate
            |> Observable.filter snd
            |> Observable.map (fun _ ->
                this.SelectedFeatureInstances
                |> Seq.toList
                |> Some
                |> SelectedFeatures)
        ]
        |> Observable.mergeSeq
        |> Observable.scanInit
            {
                TreatParenthesizedPartAsNames = this.TreatParenthesizedPartAsNames.Value
                FixupNamesInMainPart = this.FixupNamesInMainPart.Value
                RecapitalizeNames = this.RecapitalizeNames.Value
                ReplaceUnderscores = this.ReplaceUnderscores.Value
                DetectNamesInMainAndNamesParts = this.DetectNamesInMainAndNamesParts.Value
                SelectedNames = None
                SelectedFeatures = None
                Replacements = []
                AllNames = getAllNames ()
            }
            (updateParameters getAllNames)
        |> Observable.subscribe (fun parameters ->
            gate.OnNext false
            updateNewName this.OriginalFileName.Value parameters
            gate.OnNext true)
        |> ignore

        this.NewFileName
        |> Observable.combineLatest this.SelectedDestinationDirectory
        |> Observable.subscribe (fun _ -> updateResultingFilePath ())
        |> ignore

    member __.Shutdown () = saveSettings __.BaseDirectory.Value

    member __.BaseDirectory : ReactiveProperty<string> = baseDirectory

    member __.SourceDirectoryPrefixes : ReactiveProperty<_> = sourceDirectoryPrefixes

    member __.SelectedDirectory : ReactiveProperty<DirectoryInfo> = selectedDirectory

    member __.Directories = directories

    member __.Searches = searches

    member __.CreateSearchTab = Func<_> createSearchTab

    member __.ActiveSearchTab : ReactiveProperty<SearchViewModel> = activeSearchTab

    member __.SelectedFile : ReadOnlyReactiveProperty<FileInfo> = selectedFile

    member __.IsSearchEnabled = isSearchEnabled

    member __.OriginalFileName : ReactiveProperty<string> = originalFileName

    member __.NewFileName : ReactiveProperty<string> = newFileName

    member __.TreatParenthesizedPartAsNames : ReactiveProperty<bool> = treatParenthesizedPartAsNames

    member __.FixupNamesInMainPart : ReactiveProperty<bool> = fixupNamesInMainPart

    member __.ReplaceUnderscores : ReactiveProperty<bool> = replaceUnderscores

    member __.DetectNamesInMainAndNamesParts : ReactiveProperty<bool> = detectNamesInMainAndNamesParts

    member __.RecapitalizeNames : ReactiveProperty<bool> = recapitalizeNames

    member __.OpenCommand = openCommand :> ICommand
    member __.OpenExplorerCommand = openExplorerCommand :> ICommand

    member __.SelectedDestinationDirectory : ReactiveProperty<DirectoryInfo> = selectedDestinationDirectory
    member __.DestinationDirectoryPrefixes : ReactiveProperty<_> = destinationDirectoryPrefixes
    member __.DestinationDirectories = destinationDirectories

    member __.NewNameToAdd : ReactiveProperty<string> = newNameToAdd

    member __.AddNameCommand = addNameCommand :> ICommand

    member __.Names : ReactiveList<NameViewModel> = names

    member __.ResetNameSelectionCommand : ReactiveCommand = resetNameSelectionCommand

    member __.AddFeatureRoot : ReactiveProperty<bool> = addFeatureRoot

    member __.FeatureToAdd : ReactiveProperty<string> = featureToAdd

    member __.FeatureCodeToAdd : ReactiveProperty<string> = featureCodeToAdd

    member __.AddFeatureCommand = addFeatureCommand

    member __.SelectedFeature : ReactiveProperty<FeatureViewModel> = selectedFeature

    member __.SelectedFeatureInstances =
        this.FeatureInstances
        |> Seq.filter (fun vm -> vm.IsSelected)
        |> Seq.map (fun vm -> vm.FeatureCode + vm.InstanceCode)

    member __.Features : ReactiveList<FeatureViewModel> = features

    member __.FeatureInstances : ReactiveList<FeatureInstanceViewModel> = featureInstances

    member __.ResultingFilePath : ReactiveProperty<string> = resultingFilePath

    member __.ApplyCommand = applyCommand
