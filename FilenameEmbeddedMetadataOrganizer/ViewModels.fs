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

    let withLatestFrom (observable2 : IObservable<'T2>) (observable1 : IObservable<'T1>) =
        observable1.WithLatestFrom(observable2, fun v1 v2 -> v1, v2)

    let xwhen (observable2 : IObservable<_>) (observable1 : IObservable<_>) =
        observable1 |> withLatestFrom observable2 |> Observable.filter snd

    let containsAll parts (s : string) = parts |> List.forall s.Contains

    let split (separators : string []) (s : string) =
        s.Split(separators, StringSplitOptions.RemoveEmptyEntries)

    let toReadOnlyReactiveProperty (observable : IObservable<_>) =
        observable.ToReadOnlyReactiveProperty()

    let maxString maxLength (s : string) = s.Substring(0, Math.Min(s.Length, maxLength))

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

    member val IsExpanded = new ReactiveProperty<_>(true)

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

//type FeatureInstanceIncomingUpdate = X

//type FeatureInstanceOutgoingUpdate =
//    | Change of NewFeatureInstanceViewModel * FeatureInstance

//and NewFeatureInstanceViewModel(instanceName : string,
//                                instanceCode : string,
//                                incomingUpdates : IObservable<FeatureInstanceIncomingUpdate>) as this =

//    let instanceName = new ReactiveProperty<_>(instanceName)
//    let instanceCode = new ReactiveProperty<_>(instanceCode)

//    let mutable outgoingUpdates = Unchecked.defaultof<IObservable<FeatureInstanceOutgoingUpdate>>

//    do
//        outgoingUpdates <-
//            Observable.combineLatest instanceName instanceCode
//            |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
//            |> Observable.map (fun (name, code) ->
//                Change (this, { Name = name; Code = code }))

//    member __.InstanceName = instanceName
//    member __.InstanceCode = instanceCode

//    member __.Updates = outgoingUpdates

//    new (incomingUpdates : IObservable<FeatureInstanceIncomingUpdate>) =
//        NewFeatureInstanceViewModel("", "", incomingUpdates)

type NewFeatureInstanceViewModel(instanceName : string, instanceCode : string) =
    let instanceName = new ReactiveProperty<_>(instanceName)
    let instanceCode = new ReactiveProperty<_>(instanceCode)

    do
        instanceName
        |> withLatestFrom instanceCode
        |> Observable.filter (snd >> String.IsNullOrWhiteSpace)
        |> Observable.map fst
        |> Observable.subscribe (fun name -> instanceCode.Value <- maxString 2 name)
        |> ignore

    member __.InstanceName = instanceName
    member __.InstanceCode = instanceCode

    new () = NewFeatureInstanceViewModel("", "")

    new (feature : FeatureInstance) =
        NewFeatureInstanceViewModel(feature.Name, feature.Code)

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
    | Refresh

type SearchViewModel(commands : IObservable<SearchViewModelCommand>) =

    let mutable baseDirectory = ""
    let mutable selectedDirectory = Unchecked.defaultof<DirectoryInfo>

    let searchText = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let searchFromBaseDirectory = new ReactiveProperty<_>(false)
    let isActive = new ReactiveProperty<_>(true)
    let files = ObservableCollection()
    let mutable header = Unchecked.defaultof<ReadOnlyReactiveProperty<string>>
    let selectedFile = new ReactiveProperty<FileInfo>()
    let mutable refreshCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable clearSearchTextCommand = Unchecked.defaultof<ReactiveCommand>

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
                        |> containsAll
                            (toUpper searchString
                             |> split [| "&&" |]
                             |> Array.map trim
                             |> Array.toList))

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
            searchText
            |> Observable.map (function
                | "" ->
                    if isNull selectedDirectory
                    then ""
                    else sprintf "<%s>" selectedDirectory.Name
                | search -> search)
            |> Observable.startWith [ "Search" ]
            |> toReadOnlyReactiveProperty

        clearSearchTextCommand <-
            ReactiveCommand.Create(fun () -> searchText.Value <- "")

        refreshCommand <- ReactiveCommand.Create(fun () -> ignore ())

        searchText
        |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
        |> Observable.combineLatest searchFromBaseDirectory
        |> Observable.subscribe (fun (fromBaseDirectory, searchString) ->
            getFiles searchString fromBaseDirectory)
        |> ignore

        [
            refreshCommand.IsExecuting
            |> Observable.distinctUntilChanged
            |> Observable.filter id
            |> Observable.map (fun _ -> Refresh)

            commands
            |> Observable.filter (fun _ -> isActive.Value)
        ]
        |> Observable.mergeSeq
        |> Observable.subscribe (function
            | SelectedDirectory (selected, ``base``) ->
                selectedDirectory <- selected
                baseDirectory <- ``base``
                searchFromBaseDirectory.Value <- false

                searchText.Value <- ""
            | Refresh -> searchText.ForceNotify())
        |> ignore

    member __.SearchText = searchText
    member __.SearchFromBaseDirectory = searchFromBaseDirectory
    member __.IsActive = isActive
    member __.Files = files
    member __.Header = header
    member __.SelectedFile = selectedFile
    member __.RefreshCommand = refreshCommand
    member __.ClearSearchTextCommand = clearSearchTextCommand

type MainWindowViewModel() as this =
    inherit ReactiveObject()

    let baseDirectory = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let filterBySourceDirectoryPrefixes = new ReactiveProperty<_>(true)
    let sourceDirectoryPrefixes =
        new ReactiveProperty<_>("", ReactivePropertyMode.RaiseLatestValueOnSubscribe)
    let selectedDirectory =
        new ReactiveProperty<_>(Unchecked.defaultof<DirectoryInfo>, ReactivePropertyMode.None)
    let directories = ObservableCollection()

    let isSearchEnabled =
        new ReactiveProperty<bool>(selectedDirectory |> Observable.map (isNull >> not))

    let searchCommands = new ReplaySubject<SearchViewModelCommand>(1)
    let searches = ObservableCollection<SearchViewModel>()
    let activeSearchTab = new ReactiveProperty<SearchViewModel>()
    let selectedFilesSubject = new Subject<IObservable<FileInfo>>()

    let mutable selectedFile = Unchecked.defaultof<ReadOnlyReactiveProperty<FileInfo>>

    let originalFileName = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let originalFileNameSelectedText = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let newFileName = new ReactiveProperty<_>("", ReactivePropertyMode.None)

    let treatParenthesizedPartAsNames = new ReactiveProperty<_>(true)
    let fixupNamesInMainPart = new ReactiveProperty<_>(false)
    let replaceUnderscores = new ReactiveProperty<_>(true)
    let detectNamesInMainAndNamesParts = new ReactiveProperty<_>(false)
    let recapitalizeNames = new ReactiveProperty<_>(false)

    let mutable openCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable openFromSearchCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable openExplorerCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable showFilePropertiesCommand = Unchecked.defaultof<ReactiveCommand>

    let selectedDestinationDirectory =
        new ReactiveProperty<_>(Unchecked.defaultof<DirectoryInfo>, ReactivePropertyMode.None)
    let destinationDirectoryPrefixes = new ReactiveProperty<_>("")
    let destinationDirectories = ObservableCollection()

    let toReplaceToAdd = new ReactiveProperty<_>("")
    let replaceWithToAdd = new ReactiveProperty<_>("")
    let mutable addReplacementCommand = Unchecked.defaultof<ReactiveCommand>
    let replacements = ObservableCollection()

    let newNameToAdd = new ReactiveProperty<_>("")
    let mutable addNameCommand = Unchecked.defaultof<ReactiveCommand>
    let names =
        ReactiveList([], 0.5, DispatcherScheduler(Application.Current.Dispatcher), ChangeTrackingEnabled = true)
    let mutable resetNameSelectionCommand = Unchecked.defaultof<ReactiveCommand>

    let editingFeatureInstances = ObservableCollection()
    let featureToAdd = new ReactiveProperty<_>()
    let featureCodeToAdd = new ReactiveProperty<_>()
    let mutable confirmEditingFeatureCommand = Unchecked.defaultof<ReactiveCommand>
    let selectedFeature =
        new ReactiveProperty<_>(Unchecked.defaultof<FeatureViewModel>, ReactivePropertyMode.None)
    let features = ReactiveList()
    let featureInstances = ReactiveList(ChangeTrackingEnabled = true)
    let mutable removeFeatureInstanceRowCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable addFeatureInstanceRowCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable clearSelectedFeatureCommand = Unchecked.defaultof<ReactiveCommand>

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
                Replacements = this.Replacements |> Seq.toList
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

        this.Replacements.Clear()

        settings.Replacements
        |> List.iter this.Replacements.Add

        names.Clear()

        settings.Names
        |> List.iter (fun name -> NameViewModel(name, false, false) |> this.Names.Add)

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

        openFromSearchCommand <-
            ReactiveCommand.Create(fun (fi : FileInfo) -> Process.Start fi.FullName |> ignore)

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

        showFilePropertiesCommand <-
            ReactiveCommand.Create(fun (fi : FileInfo) ->
                Interop.showFileProperties fi.FullName |> ignore)

        addReplacementCommand <-
            ReactiveCommand.Create(
                (fun () ->
                    { ToReplace = this.ToReplaceToAdd.Value; ReplaceWith = this.ReplaceWithToAdd.Value }
                    |> this.Replacements.Add

                    this.ToReplaceToAdd.Value <- ""
                    this.ReplaceWithToAdd.Value <- ""),
                this.ToReplaceToAdd |> Observable.map (String.IsNullOrWhiteSpace >> not))

        addNameCommand <-
            ReactiveCommand.Create(fun name ->
                if not <| String.IsNullOrWhiteSpace name
                then
                    NameViewModel(trim name, false, false) |> this.Names.Add
                    this.NewNameToAdd.Value <- "")

        resetNameSelectionCommand <- ReactiveCommand.Create(fun () -> ignore ())

        confirmEditingFeatureCommand <-
            ReactiveCommand.Create(fun () ->
                if not <| String.IsNullOrWhiteSpace this.FeatureToAdd.Value
                    && not <| String.IsNullOrWhiteSpace this.FeatureCodeToAdd.Value
                then
                    let feature =
                        FeatureViewModel({ Name = this.FeatureToAdd.Value; Code = this.FeatureCodeToAdd.Value; Instances = [] })

                    this.EditingFeatureInstances
                    |> Seq.filter (fun vm ->
                        not <| String.IsNullOrWhiteSpace vm.InstanceName.Value
                        && not <| String.IsNullOrWhiteSpace vm.InstanceCode.Value)
                    |> Seq.iter (fun vm ->
                        let instance =
                            FeatureInstanceViewModel(feature.Feature, { Name = vm.InstanceName.Value; Code = vm.InstanceCode.Value })

                        feature.Instances.Add instance)

                    this.FeatureToAdd.Value <- ""
                    this.FeatureCodeToAdd.Value <- ""

                    this.EditingFeatureInstances.Clear()
                    NewFeatureInstanceViewModel() |> this.EditingFeatureInstances.Add

                    this.SelectedFeature.Value
                    |> Option.ofObj
                    |> Option.iter (fun feature ->

                        feature.Instances
                        |> Seq.iter (this.FeatureInstances.Remove >> ignore)

                        this.Features.Remove feature |> ignore

                        this.SelectedFeature.Value <- Unchecked.defaultof<FeatureViewModel>)

                    features.Add feature
                    feature.Instances |> Seq.iter this.FeatureInstances.Add)

        removeFeatureInstanceRowCommand <-
            ReactiveCommand.Create(fun (vm : NewFeatureInstanceViewModel) ->
                this.EditingFeatureInstances.Remove vm |> ignore)

        addFeatureInstanceRowCommand <-
            ReactiveCommand.Create(fun () ->
                NewFeatureInstanceViewModel() |> this.EditingFeatureInstances.Add)

        clearSelectedFeatureCommand <-
            ReactiveCommand.Create(fun () ->
                this.SelectedFeature.Value <- Unchecked.defaultof<FeatureViewModel>)

        applyCommand <-
            ReactiveCommand.Create(
                (fun () ->
                    File.Move(this.SelectedFile.Value.FullName, this.ResultingFilePath.Value)
                    searchCommands.OnNext Refresh),
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
        |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
        |> Observable.combineLatest this.FilterBySourceDirectoryPrefixes
        |> Observable.combineLatest
            (this.BaseDirectory
             |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
             |> Observable.filter Directory.Exists)
        |> Observable.subscribe (fun (dir, (filterByPrefixes, prefixes)) ->
            directories.Clear()

            Directory.GetDirectories dir
            |> Seq.map DirectoryInfo
            |> Seq.filter (fun di ->
                match filterByPrefixes, prefixes with
                | false, _ | _, "" -> true
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

        existingSelectedFile
        |> Observable.subscribe (fun fi ->
            this.Names
            |> Seq.filter (fun vm -> vm.IsNew.Value)
            |> Seq.toList
            |> List.iter (this.Names.Remove >> ignore)

            this.OriginalFileName.Value <-
                string fi.Name |> Path.GetFileNameWithoutExtension)
        |> ignore

        this.OriginalFileNameSelectedText
        |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
        |> Observable.subscribe (fun text -> this.NewNameToAdd.Value <- text)
        |> ignore

        NewFeatureInstanceViewModel()
        |> this.EditingFeatureInstances.Add

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
            (updateParameters this.Replacements getAllNames)
        |> Observable.subscribe (fun parameters ->
            gate.OnNext false
            updateNewName this.OriginalFileName.Value parameters
            gate.OnNext true)
        |> ignore

        this.NewFileName
        |> Observable.combineLatest this.SelectedDestinationDirectory
        |> Observable.subscribe (fun _ -> updateResultingFilePath ())
        |> ignore

        this.SelectedFeature
        |> Observable.subscribe (fun (OfNull feature) ->
            this.EditingFeatureInstances.Clear()

            let featureName, featureCode =
                feature
                |> Option.map (fun feature -> feature.FeatureName, feature.FeatureCode)
                |> Option.defaultValue ("", "")

            this.FeatureToAdd.Value <- featureName
            this.FeatureCodeToAdd.Value <- featureCode

            feature
            |> Option.bind (fun feature ->
                feature.Instances
                |> Seq.map (fun instance -> instance.Instance)
                |> Seq.toList
                |> function
                    | [] -> None
                    | instances ->
                        instances
                        |> List.map NewFeatureInstanceViewModel
                        |> Some)
            |> Option.defaultWith (fun () -> [ NewFeatureInstanceViewModel() ])
            |> List.iter this.EditingFeatureInstances.Add)
        |> ignore

    member __.Shutdown () = saveSettings __.BaseDirectory.Value

    member __.BaseDirectory : ReactiveProperty<string> = baseDirectory
    member __.FilterBySourceDirectoryPrefixes = filterBySourceDirectoryPrefixes
    member __.SourceDirectoryPrefixes : ReactiveProperty<_> = sourceDirectoryPrefixes

    member __.SelectedDirectory : ReactiveProperty<DirectoryInfo> = selectedDirectory

    member __.Directories = directories

    member __.Searches = searches

    member __.CreateSearchTab = Func<_> createSearchTab

    member __.ActiveSearchTab : ReactiveProperty<SearchViewModel> = activeSearchTab

    member __.SelectedFile : ReadOnlyReactiveProperty<FileInfo> = selectedFile

    member __.IsSearchEnabled = isSearchEnabled

    member __.OriginalFileName : ReactiveProperty<string> = originalFileName
    member __.OriginalFileNameSelectedText : ReactiveProperty<_> = originalFileNameSelectedText
    member __.NewFileName : ReactiveProperty<string> = newFileName

    member __.TreatParenthesizedPartAsNames : ReactiveProperty<bool> = treatParenthesizedPartAsNames

    member __.FixupNamesInMainPart : ReactiveProperty<bool> = fixupNamesInMainPart

    member __.ReplaceUnderscores : ReactiveProperty<bool> = replaceUnderscores

    member __.DetectNamesInMainAndNamesParts : ReactiveProperty<_> = detectNamesInMainAndNamesParts

    member __.RecapitalizeNames : ReactiveProperty<bool> = recapitalizeNames

    member __.OpenCommand = openCommand :> ICommand
    member __.OpenFromSearchCommand = openFromSearchCommand :> ICommand
    member __.OpenExplorerCommand = openExplorerCommand :> ICommand
    member __.ShowFilePropertiesCommand = showFilePropertiesCommand

    member __.SelectedDestinationDirectory : ReactiveProperty<_> = selectedDestinationDirectory
    member __.DestinationDirectoryPrefixes : ReactiveProperty<_> = destinationDirectoryPrefixes
    member __.DestinationDirectories = destinationDirectories

    member __.ToReplaceToAdd : ReactiveProperty<_> = toReplaceToAdd
    member __.ReplaceWithToAdd : ReactiveProperty<_> = replaceWithToAdd
    member __.AddReplacementCommand = addReplacementCommand
    member __.Replacements : ObservableCollection<_> = replacements

    member __.NewNameToAdd : ReactiveProperty<string> = newNameToAdd

    member __.AddNameCommand = addNameCommand :> ICommand

    member __.Names : ReactiveList<NameViewModel> = names

    member __.ResetNameSelectionCommand : ReactiveCommand = resetNameSelectionCommand

    member __.EditingFeatureInstances : ObservableCollection<NewFeatureInstanceViewModel> = editingFeatureInstances
    member __.RemoveFeatureInstanceRowCommand = removeFeatureInstanceRowCommand
    member __.AddFeatureInstanceRowCommand = addFeatureInstanceRowCommand
    member __.ClearSelectedFeatureCommand = clearSelectedFeatureCommand
    member __.AddFeatureInstanceRow() =
        NewFeatureInstanceViewModel() |> this.EditingFeatureInstances.Add

    member __.FeatureToAdd : ReactiveProperty<string> = featureToAdd

    member __.FeatureCodeToAdd : ReactiveProperty<string> = featureCodeToAdd

    member __.ConfirmEditingFeatureCommand = confirmEditingFeatureCommand

    member __.SelectedFeature : ReactiveProperty<FeatureViewModel> = selectedFeature

    member __.SelectedFeatureInstances =
        this.FeatureInstances
        |> Seq.filter (fun vm -> vm.IsSelected)
        |> Seq.map (fun vm -> vm.FeatureCode + vm.InstanceCode)

    member __.Features : ReactiveList<FeatureViewModel> = features

    member __.FeatureInstances : ReactiveList<FeatureInstanceViewModel> = featureInstances

    member __.ResultingFilePath : ReactiveProperty<string> = resultingFilePath

    member __.ApplyCommand = applyCommand
