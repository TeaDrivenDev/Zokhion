namespace FilenameEmbeddedMetadataOrganizer.ViewModels

open System
open System.Collections.ObjectModel
open System.Diagnostics
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Windows

open Dragablz

open FSharp.Control.Reactive

open ReactiveUI

open Reactive.Bindings
open Reactive.Bindings.Notifiers

open FilenameEmbeddedMetadataOrganizer

type ReactiveCommand = ReactiveUI.ReactiveCommand

[<AutoOpen>]
module Utility =
    open System.Linq.Expressions
    open FSharp.Quotations

    // see https://stackoverflow.com/a/48311816/236507
    let nameof (q : Expr<_>) =
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

    /// Converts a Lambda quotation into a Linq Lambda Expression with 1 parameter
    let toLinq (exp : Expr<'a -> 'b>) =
        let lambda = toLambda exp
        Expression.Lambda<Func<'a, 'b>>(lambda.Body, lambda.Parameters)

    let withLatestFrom (observable2 : IObservable<'T2>) (observable1 : IObservable<'T1>) =
        observable1.WithLatestFrom(observable2, fun v1 v2 -> v1, v2)

    let xwhen (observable2 : IObservable<_>) (observable1 : IObservable<_>) =
        observable1 |> withLatestFrom observable2 |> Observable.filter snd |> Observable.map fst

    let containsAll parts (s : string) = parts |> List.forall s.Contains

    let split (separators : string []) (s : string) =
        s.Split(separators, StringSplitOptions.RemoveEmptyEntries)

    let toReadOnlyReactiveProperty (observable : IObservable<_>) =
        observable.ToReadOnlyReactiveProperty()

    let maxString maxLength (s : string) = s.Substring(0, Math.Min(s.Length, maxLength))

[<AllowNullLiteral>]
type FeatureViewModel(feature : Feature) as this =
    inherit ReactiveObject()

    let instances = ReactiveList<FeatureInstanceViewModel>(ChangeTrackingEnabled = true)

    let mutable hasSelectedInstances = Unchecked.defaultof<ReadOnlyReactiveProperty<_>>

    do
        match this with
        | :? FeatureInstanceViewModel -> ()
        | _ ->
            feature.Instances
            |> List.map (fun instance -> FeatureInstanceViewModel(feature, instance))
            |> instances.AddRange

        hasSelectedInstances <-
            instances.ItemChanged
            |> Observable.filter (fun change ->
                change.PropertyName = nameof <@ any<FeatureInstanceViewModel>.IsSelected @>)
            |> Observable.map (fun _ ->
                instances |> Seq.exists (fun instance -> instance.IsSelected))
            |> toReadOnlyReactiveProperty

    member __.FeatureName = feature.Name

    member __.FeatureCode = feature.Code

    member __.Include = feature.Include

    member __.Instances = instances

    member __.HasSelectedInstances = hasSelectedInstances

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
type NameViewModel(name : string, isSelected : bool, isNewlyDetected : bool, isAdded : bool) =
    inherit ReactiveObject()

    let mutable xIsSelected = isSelected

    member val Name = new ReactiveProperty<_>(name)

    member __.IsSelected
        with get () = xIsSelected
        and set value = __.RaiseAndSetIfChanged(&xIsSelected, value, nameof <@ __.IsSelected @>) |> ignore

    member val IsNewlyDetected = new ReactiveProperty<_>(isNewlyDetected)

    member val IsAdded = new ReactiveProperty<_>(isAdded)

    member __.ClearNewFlagCommand =
        ReactiveCommand.Create(fun () ->
            __.IsNewlyDetected.Value <- false
            __.IsAdded.Value <- true)

type SearchViewModelCommand =
    | SelectedDirectory of (DirectoryInfo option * string)
    | Refresh

type SearchCriterion =
    | Contains of string list
    | SmallerThan of int
    | LargerThan of int

type SearchViewModel(commands : IObservable<SearchViewModelCommand>) =

    let mutable baseDirectory = ""
    let selectedDirectory = new BehaviorSubject<DirectoryInfo option>(None)

    let searchText = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let searchFromBaseDirectory = new ReactiveProperty<_>(true)
    let mutable canToggleSearchFromBaseDirectory =
        Unchecked.defaultof<ReadOnlyReactiveProperty<bool>>
    let isActive = new ReactiveProperty<_>(true)
    let files = ObservableCollection()
    let mutable header = Unchecked.defaultof<ReadOnlyReactiveProperty<string>>
    let selectedFile = new ReactiveProperty<FileInfo>()
    let mutable refreshCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable clearSearchTextCommand = Unchecked.defaultof<ReactiveCommand>
    let isUpdating = BooleanNotifier(false)

    let smallerThanRegex = System.Text.RegularExpressions.Regex(@"^<\s*(?<size>\d+)MB$")
    let largerThanRegex = System.Text.RegularExpressions.Regex(@"^>\s*(?<size>\d+)MB$")

    let getFiles searchString fromBaseDirectory =
        let filter searchString =
            if String.IsNullOrWhiteSpace searchString
            then (fun _ -> true)
            else
                let smaller, contains, larger =
                    searchString
                    |> toUpper
                    |> split [| "&&" |]
                    |> Array.map trim
                    |> Array.toList
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

                let filters =
                    [
                        yield
                            smaller
                            |> Option.map (fun smaller -> fun (fi : FileInfo) -> fi.Length < int64 smaller)

                        yield
                            larger
                            |> Option.map (fun larger -> fun (fi : FileInfo) -> fi.Length > int64 larger)

                        yield
                            match contains with
                            | [] -> None
                            | contains ->
                                (fun (fi : FileInfo) ->
                                    fi.Name
                                    |> Path.GetFileNameWithoutExtension
                                    |> toUpper
                                    |> (fun s -> [ s; s.Replace("_", " ") ])
                                    |> Seq.exists (containsAll contains))
                                    |> Some
                    ]
                    |> List.choose id

                fun (fi : FileInfo) -> filters |> Seq.forall (fun filter -> filter fi)

        if fromBaseDirectory && not <| String.IsNullOrWhiteSpace searchString
        then Some baseDirectory
        else selectedDirectory.Value |> Option.map (fun selected -> selected.FullName)
        |> Option.map (fun dir ->
            Directory.GetFiles(dir, "*", SearchOption.AllDirectories)
            |> Seq.map FileInfo
            |> Seq.filter (filter searchString))

    do
        header <-
            searchText
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

        clearSearchTextCommand <- ReactiveCommand.Create(fun () -> searchText.Value <- "")

        refreshCommand <- ReactiveCommand.Create(fun () -> ignore ())

        searchText
        |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
        |> Observable.combineLatest searchFromBaseDirectory
        |> Observable.iter (fun _ -> isUpdating.TurnOn())
        |> Observable.observeOn ThreadPoolScheduler.Instance
        |> Observable.choose (fun (fromBaseDirectory, searchString) ->
            getFiles searchString fromBaseDirectory)
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe (fun newFiles ->
            files.Clear()

            newFiles |> Seq.iter files.Add

            isUpdating.TurnOff())
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
                selectedDirectory.OnNext selected
                baseDirectory <- ``base``
                searchFromBaseDirectory.Value <- Option.isNone selected

                searchText.Value <- ""
            | Refresh -> searchText.ForceNotify())
        |> ignore

    member __.SearchText = searchText
    member __.SearchFromBaseDirectory = searchFromBaseDirectory
    member __.CanToggleSearchFromBaseDirectory = canToggleSearchFromBaseDirectory
    member __.IsActive = isActive
    member __.Files = files
    member __.Header = header
    member __.SelectedFile = selectedFile
    member __.RefreshCommand = refreshCommand
    member __.ClearSearchTextCommand = clearSearchTextCommand
    member __.IsUpdating = isUpdating

type MainWindowViewModel() as this =
    inherit ReactiveObject()

    let baseDirectory = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let filterBySourceDirectoryPrefixes = new ReactiveProperty<_>(true)
    let sourceDirectoryPrefixes =
        new ReactiveProperty<_>("", ReactivePropertyMode.RaiseLatestValueOnSubscribe)
    let selectedDirectory =
        new ReactiveProperty<_>(Unchecked.defaultof<DirectoryInfo>, ReactivePropertyMode.None)
    let directories = ObservableCollection()
    let mutable refreshDirectoriesCommand = Unchecked.defaultof<ReactiveCommand>

    let mutable isBaseDirectoryValid = Unchecked.defaultof<ReadOnlyReactiveProperty<bool>>

    let searchCommands = new ReplaySubject<SearchViewModelCommand>(1)
    let searches = ObservableCollection<SearchViewModel>()
    let activeSearchTab = new ReactiveProperty<SearchViewModel>()
    let selectedFilesSubject = new System.Reactive.Subjects.Subject<IObservable<FileInfo>>()

    let mutable selectedFile = Unchecked.defaultof<ReadOnlyReactiveProperty<FileInfo>>

    let mutable showSettings = new ReactiveProperty<_>(false)
    let mutable saveSettingsCommand = Unchecked.defaultof<ReactiveCommand>

    let originalFileName = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let originalFileNameSelectedText = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let newFileName = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let newFileNameSelectedText = new ReactiveProperty<_>("", ReactivePropertyMode.None)

    let treatParenthesizedPartAsNames = new ReactiveProperty<_>(true)
    let fixupNamesInMainPart = new ReactiveProperty<_>(false)
    let replaceUnderscores = new ReactiveProperty<_>(true)
    let detectNamesInMainAndNamesParts = new ReactiveProperty<_>(false)
    let recapitalizeNames = new ReactiveProperty<_>(false)

    let mutable openCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable openFromSearchCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable openExplorerCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable showFilePropertiesCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable deleteFileCommand = Unchecked.defaultof<ReactiveCommand>

    let selectedDestinationDirectory =
        new ReactiveProperty<_>(Unchecked.defaultof<DirectoryInfo>, ReactivePropertyMode.None)
    let destinationDirectoryPrefixes = new ReactiveProperty<_>("")
    let destinationDirectories = ObservableCollection()

    let toReplaceToAdd = new ReactiveProperty<_>("")
    let replaceWithToAdd = new ReactiveProperty<_>("")
    let mutable addReplacementCommand = Unchecked.defaultof<ReactiveCommand>
    let replacements = ObservableCollection()

    let newNameToAdd = new ReactiveProperty<_>("", ReactivePropertyMode.RaiseLatestValueOnSubscribe)
    let mutable clearNewNameToAddCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable addNameCommand = Unchecked.defaultof<ReactiveCommand>
    let allNames =
        ReactiveList<NameViewModel>([], 0.5, DispatcherScheduler(Application.Current.Dispatcher), ChangeTrackingEnabled = true)
    let names = ObservableCollection()
    let mutable resetNameSelectionCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable searchForTextCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable searchForNameCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable deleteNameCommand = Unchecked.defaultof<ReactiveCommand>

    let editingFeatureInstances = ObservableCollection()
    let editingFeatureName = new ReactiveProperty<_>()
    let editingFeatureCode = new ReactiveProperty<_>()
    let editingFeatureToInclude = new ReactiveProperty<_>()
    let mutable confirmEditingFeatureCommand = Unchecked.defaultof<ReactiveCommand>
    let selectedFeature =
        new ReactiveProperty<_>(Unchecked.defaultof<FeatureViewModel>, ReactivePropertyMode.None)
    let features = ReactiveList()
    let featureInstances = ReactiveList(ChangeTrackingEnabled = true)
    let isFeatureDragDropReorderingEnabled = new ReactiveProperty<_>()
    let mutable removeFeatureInstanceRowCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable addFeatureInstanceRowCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable clearSelectedFeatureCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable expandAllFeaturesCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable collapseAllFeaturesCommand = Unchecked.defaultof<ReactiveCommand>

    let resultingFilePath = new ReactiveProperty<_>("", ReactivePropertyMode.None)
    let mutable applyCommand = Unchecked.defaultof<ReactiveCommand>

    let updateDirectoriesList baseDirectory prefixes filterByPrefixes =
        directories.Clear()

        Directory.GetDirectories baseDirectory
        |> Seq.map DirectoryInfo
        |> Seq.filter (fun di ->
            match filterByPrefixes, prefixes with
            | false, _ | _, "" -> true
            | _ -> prefixes |> Seq.exists (string >> di.Name.StartsWith))
        |> Seq.sortWith (fun x y -> Interop.StrCmpLogicalW(x.Name, y.Name))
        |> Seq.iter directories.Add

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
        |> Array.sortWith (fun x y -> Interop.StrCmpLogicalW(x, y))
        |> Array.toList)
        |> List.distinct
        |> List.map DirectoryInfo
        |> List.iter destinationDirectories.Add

        this.SelectedDestinationDirectory.Value <-
            destinationDirectories
            |> Seq.find (fun (d : DirectoryInfo) -> d.FullName = currentFileDirectory)

    let clearNewNameToAdd () = this.NewNameToAdd.Value <- ""

    let addName name =
        if not <| String.IsNullOrWhiteSpace name
        then
            let name = trim name

            let viewModel =
                allNames
                |> Seq.tryFind (fun vm -> vm.Name.Value = name)
                |> Option.defaultWith (fun () ->
                    let vm = NameViewModel(trim name, false, false, true)
                    allNames.Add vm
                    vm)

            viewModel.IsSelected <- true

            this.NewNameToAdd.Value <- ""

    let updateNamesList detectedNames =
        (detectedNames, allNames)
        ||> fullOuterJoin toUpper (fun vm -> vm.Name.Value |> toUpper)
        |> Seq.iter (function
            | LeftOnly vm -> vm.IsSelected <- false
            | RightOnly name ->
                NameViewModel(name, true, true, false)
                |> allNames.Add
            | JoinMatch (vm, name) ->
                vm.Name.Value <- name
                vm.IsSelected <- true)

    let updateNamesSearchResult name =
        this.Names.Clear()

        match name with
        | "" -> allNames :> _ seq
        | _ ->
            let up = toUpper name

            allNames |> Seq.filter (fun n -> n.Name.Value.ToUpper().Contains up)
        |> Seq.iter (this.Names.Add)

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
        allNames
        |> Seq.filter (fun vm -> not vm.IsNewlyDetected.Value)
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
                    allNames
                    |> Seq.filter (fun vm -> not vm.IsNewlyDetected.Value)
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

        allNames.Clear()

        settings.Names
        |> List.iter (fun name -> NameViewModel(name, false, false, false) |> allNames.Add)

        updateNamesSearchResult ""

        features.Clear()
        featureInstances.Clear()

        settings.Features
        |> List.iter (FeatureViewModel >> this.Features.Add)

        this.Features
        |> Seq.collect (fun vm -> vm.Instances)
        |> this.FeatureInstances.AddRange

    let createSearchTab directory searchText =
        let search = SearchViewModel(searchCommands.AsObservable())

        selectedFilesSubject.OnNext search.SelectedFile
        this.ActiveSearchTab.Value <- search

        directory
        |> Option.iter (fun dir ->
            SelectedDirectory(Some (DirectoryInfo dir), this.BaseDirectory.Value)
            |> searchCommands.OnNext)

        searchText
        |> Option.iter (fun text ->
            search.SearchFromBaseDirectory.Value <- true
            search.SearchText.Value <- text)

        search

    do
        RxApp.MainThreadScheduler <- DispatcherScheduler(Application.Current.Dispatcher)

        refreshDirectoriesCommand <-
            ReactiveCommand.Create(fun () ->
                updateDirectoriesList
                    this.BaseDirectory.Value
                    this.SourceDirectoryPrefixes.Value
                    this.FilterBySourceDirectoryPrefixes.Value)

        selectedFile <-
            selectedFilesSubject
            |> Observable.flatmap id
            |> toReadOnlyReactiveProperty

        saveSettingsCommand <- ReactiveCommand.Create(fun () -> saveSettings this.BaseDirectory.Value)

        openCommand <-
            ReactiveCommand.Create(
                (fun (fi : FileInfo) -> Process.Start fi.FullName |> ignore),
                this.SelectedFile |> Observable.map (fun fi -> not <| isNull fi && fi.Exists))

        openFromSearchCommand <-
            ReactiveCommand.Create(fun (fi : FileInfo) ->
                if fi.Exists then Process.Start fi.FullName |> ignore)

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
                if fi.Exists then Interop.showFileProperties fi.FullName |> ignore)

        deleteFileCommand <-
            ReactiveCommand.Create(fun (fi : FileInfo) ->
                if fi.Exists
                then
                    MessageBox.Show(sprintf "Delete file %s?" fi.FullName,
                                        "Delete File",
                                        MessageBoxButton.OKCancel,
                                        MessageBoxImage.Question)
                    |> function
                        | MessageBoxResult.OK ->
                            try
                                fi.Delete()
                            with _ ->
                                MessageBox.Show(sprintf "Could not delete file %s." fi.FullName,
                                                "Delete Failed",
                                                MessageBoxButton.OK,
                                                MessageBoxImage.Error)
                                |> ignore

                            searchCommands.OnNext Refresh
                        | _ -> ())

        addReplacementCommand <-
            ReactiveCommand.Create(
                (fun () ->
                    { ToReplace = this.ToReplaceToAdd.Value; ReplaceWith = this.ReplaceWithToAdd.Value }
                    |> this.Replacements.Add

                    this.ToReplaceToAdd.Value <- ""
                    this.ReplaceWithToAdd.Value <- ""),
                this.ToReplaceToAdd |> Observable.map (String.IsNullOrWhiteSpace >> not))

        clearNewNameToAddCommand <- ReactiveCommand.Create clearNewNameToAdd

        addNameCommand <- ReactiveCommand.Create addName

        resetNameSelectionCommand <- ReactiveCommand.Create(fun () -> ignore ())

        searchForTextCommand <-
            ReactiveCommand.Create(fun name -> createSearchTab None (Some name) |> searches.Add)

        searchForNameCommand <-
            ReactiveCommand.Create(fun name ->
                sprintf ".%s." name |> Some |> createSearchTab None |> searches.Add)

        deleteNameCommand <-
            ReactiveCommand.Create(fun (vm : NameViewModel) ->
                MessageBox.Show(sprintf "Delete name '%s'?" vm.Name.Value,
                                "Delete Name",
                                MessageBoxButton.OKCancel,
                                MessageBoxImage.Question)
                |> function
                    | MessageBoxResult.OK ->
                        allNames.Remove vm |> ignore
                        updateNamesSearchResult ""
                    | _ -> ())

        confirmEditingFeatureCommand <-
            ReactiveCommand.Create(fun () ->
                if not <| String.IsNullOrWhiteSpace this.EditingFeatureName.Value
                    && not <| String.IsNullOrWhiteSpace this.EditingFeatureCode.Value
                then
                    let feature =
                        FeatureViewModel(
                            {
                                Name = this.EditingFeatureName.Value
                                Code = this.EditingFeatureCode.Value
                                Include = nonEmptyString this.EditingFeatureToInclude.Value
                                Instances = []
                            })

                    this.EditingFeatureInstances
                    |> Seq.filter (fun vm ->
                        not <| String.IsNullOrWhiteSpace vm.InstanceName.Value
                        && not <| String.IsNullOrWhiteSpace vm.InstanceCode.Value)
                    |> Seq.iter (fun vm ->
                        let instance =
                            FeatureInstanceViewModel(feature.Feature, { Name = vm.InstanceName.Value; Code = vm.InstanceCode.Value })

                        feature.Instances.Add instance)

                    this.EditingFeatureName.Value <- ""
                    this.EditingFeatureCode.Value <- ""

                    this.EditingFeatureInstances.Clear()
                    NewFeatureInstanceViewModel() |> this.EditingFeatureInstances.Add

                    let index =
                        this.SelectedFeature.Value
                        |> Option.ofObj
                        |> Option.map (fun feature ->
                            feature.Instances
                            |> Seq.iter (this.FeatureInstances.Remove >> ignore)

                            let index = this.Features.IndexOf feature
                            this.Features.Remove feature |> ignore

                            this.SelectedFeature.Value <- Unchecked.defaultof<FeatureViewModel>

                            index)

                    match index |> Option.defaultValue -1 with
                    | -1 -> features.Add feature
                    | index -> features.Insert(index, feature)

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

        expandAllFeaturesCommand <-
            ReactiveCommand.Create(fun () ->
                this.Features |> Seq.iter (fun vm -> vm.IsExpanded.Value <- true))

        collapseAllFeaturesCommand <-
            ReactiveCommand.Create(fun () ->
                this.Features |> Seq.iter (fun vm -> vm.IsExpanded.Value <- false))

        applyCommand <-
            ReactiveCommand.Create(
                (fun () ->
                    try
                        File.Move(this.SelectedFile.Value.FullName, this.ResultingFilePath.Value)
                        searchCommands.OnNext Refresh
                    with
                    | _ ->
                        MessageBox.Show("Renaming failed. Please make sure the file is not in use by another application.",
                                        "Renaming Failed",
                                        MessageBoxButton.OK,
                                        MessageBoxImage.Warning)
                        |> ignore),
                [
                    this.SelectedFile |> Observable.map (fun fi -> not <| isNull fi && fi.Exists)
                    this.ResultingFilePath |> Observable.map (fun path -> not <| isNull path && not <| File.Exists path)
                ]
                |> Observable.combineLatestSeq
                |> Observable.map (Seq.toList >> List.forall id))

        this.SelectedDirectory
        |> Observable.map (fun dir -> SelectedDirectory (Option.ofObj dir, this.BaseDirectory.Value))
        |> Observable.subscribeObserver searchCommands
        |> ignore

        this.ActiveSearchTab
        |> Observable.subscribe (fun tab ->
            searches
            |> Seq.iter (fun vm -> vm.IsActive.Value <- (tab = vm)))
        |> ignore

        let validBaseDirectory =
            this.BaseDirectory
            |> Observable.throttle (TimeSpan.FromMilliseconds 500.)
            |> Observable.filter Directory.Exists

        validBaseDirectory
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe (fun dir ->
            loadSettings dir
            searchCommands.OnNext (SelectedDirectory(None, dir)))
        |> ignore

        isBaseDirectoryValid <-
            this.BaseDirectory
            |> Observable.combineLatest validBaseDirectory
            |> Observable.map (fun (``base``, validBase) -> ``base`` = validBase)
            |> toReadOnlyReactiveProperty

        this.SourceDirectoryPrefixes
        |> Observable.throttle (TimeSpan.FromMilliseconds 500.)
        |> Observable.combineLatest this.FilterBySourceDirectoryPrefixes
        |> Observable.combineLatest validBaseDirectory
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe (fun (baseDirectory, (filterByPrefixes, prefixes)) ->
            updateDirectoriesList baseDirectory prefixes filterByPrefixes)
        |> ignore

        createSearchTab None None |> searches.Add

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
            allNames
            |> Seq.filter (fun vm -> vm.IsNewlyDetected.Value)
            |> Seq.toList
            |> List.iter (allNames.Remove >> ignore)

            this.OriginalFileName.Value <-
                string fi.Name |> Path.GetFileNameWithoutExtension

            this.NewNameToAdd.Value <- "")
        |> ignore

        [
            this.OriginalFileNameSelectedText :> IObservable<_>
            this.NewFileNameSelectedText :> IObservable<_>
        ]
        |> Observable.mergeSeq
        |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromMilliseconds 500.)
        |> Observable.subscribe (fun text ->
            this.NewNameToAdd.Value <-
                if this.RecapitalizeNames.Value
                then toTitleCase text
                else text)
        |> ignore

        NewFeatureInstanceViewModel()
        |> this.EditingFeatureInstances.Add

        let updateNewNameGate = new BooleanNotifier(true)

        [
            this.TreatParenthesizedPartAsNames |> Observable.map TreatParenthesizedPartAsNames
            this.FixupNamesInMainPart |> Observable.map FixupNamesInMainPart
            this.ReplaceUnderscores |> Observable.map ReplaceUnderscores
            this.DetectNamesInMainAndNamesParts |> Observable.map DetectNamesInMainAndNamesParts
            this.RecapitalizeNames |> Observable.map RecapitalizeNames

            allNames.ItemChanged
            |> Observable.filter (fun change ->
                change.PropertyName = nameof <@ any<NameViewModel>.IsSelected @>)
            |> xwhen updateNewNameGate
            |> Observable.map (fun _ ->
                allNames
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
            |> xwhen updateNewNameGate
            |> Observable.iter (fun change ->
                if change.Sender.IsSelected
                then
                    updateNewNameGate.TurnOff()

                    change.Sender.Include
                    |> Option.iter (fun toInclude ->
                        let featureToInclude =
                            this.FeatureInstances
                            |> Seq.tryFind (fun vm -> vm.CompositeInstanceCode = toInclude)

                        featureToInclude
                        |> Option.iter (fun vm -> vm.IsSelected <- true))

                    updateNewNameGate.TurnOn())
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
            updateNewNameGate.TurnOff()
            updateNewName this.OriginalFileName.Value parameters
            updateNewNameGate.TurnOn())
        |> ignore

        this.NewNameToAdd
        |> Observable.throttleOn ThreadPoolScheduler.Instance (TimeSpan.FromMilliseconds 500.)
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe updateNamesSearchResult
        |> ignore

        this.NewFileName
        |> Observable.combineLatest this.SelectedDestinationDirectory
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe (fun _ -> updateResultingFilePath ())
        |> ignore

        this.SelectedFeature
        |> Observable.subscribe (fun (OfNull feature) ->
            this.EditingFeatureInstances.Clear()

            let featureName, featureCode, toInclude =
                feature
                |> Option.map (fun feature -> feature.FeatureName, feature.FeatureCode, feature.Include)
                |> Option.defaultValue ("", "", None)

            this.EditingFeatureName.Value <- featureName
            this.EditingFeatureCode.Value <- featureCode
            this.EditingFeatureToInclude.Value <- toInclude |> Option.defaultValue ""

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
    member __.FilterBySourceDirectoryPrefixes : ReactiveProperty<_> = filterBySourceDirectoryPrefixes
    member __.SourceDirectoryPrefixes : ReactiveProperty<_> = sourceDirectoryPrefixes
    member __.RefreshDirectoriesCommand = refreshDirectoriesCommand

    member __.SelectedDirectory : ReactiveProperty<DirectoryInfo> = selectedDirectory

    member __.Directories = directories

    member __.Searches = searches
    member __.CreateSearchTab = Func<_>(fun () -> createSearchTab None None)
    member __.CreateSearchTabForDirectory(directory : string) =
        createSearchTab (Some directory) None |> searches.Add
    member __.ActiveSearchTab : ReactiveProperty<SearchViewModel> = activeSearchTab
    member __.IsBaseDirectoryValid : ReadOnlyReactiveProperty<_> = isBaseDirectoryValid
    member __.CloseSearchTabCallback =
        ItemActionCallback(fun (args : ItemActionCallbackArgs<TabablzControl>) ->
            if args.Owner.Items.Count < 2 then args.Cancel())

    member __.SelectedFile : ReadOnlyReactiveProperty<FileInfo> = selectedFile

    member __.ShowSettings = showSettings
    member __.SaveSettingsCommand = saveSettingsCommand

    member __.OriginalFileName : ReactiveProperty<string> = originalFileName
    member __.OriginalFileNameSelectedText : ReactiveProperty<_> = originalFileNameSelectedText
    member __.NewFileName : ReactiveProperty<string> = newFileName
    member __.NewFileNameSelectedText : ReactiveProperty<_> = newFileNameSelectedText

    member __.TreatParenthesizedPartAsNames : ReactiveProperty<_> = treatParenthesizedPartAsNames
    member __.FixupNamesInMainPart : ReactiveProperty<_> = fixupNamesInMainPart
    member __.ReplaceUnderscores : ReactiveProperty<_> = replaceUnderscores
    member __.DetectNamesInMainAndNamesParts : ReactiveProperty<_> = detectNamesInMainAndNamesParts
    member __.RecapitalizeNames : ReactiveProperty<_> = recapitalizeNames

    member __.OpenCommand = openCommand
    member __.OpenFromSearchCommand = openFromSearchCommand
    member __.OpenExplorerCommand = openExplorerCommand
    member __.ShowFilePropertiesCommand = showFilePropertiesCommand
    member __.DeleteFileCommand = deleteFileCommand

    member __.SelectedDestinationDirectory : ReactiveProperty<_> = selectedDestinationDirectory
    member __.DestinationDirectoryPrefixes : ReactiveProperty<_> = destinationDirectoryPrefixes
    member __.DestinationDirectories = destinationDirectories

    member __.ToReplaceToAdd : ReactiveProperty<_> = toReplaceToAdd
    member __.ReplaceWithToAdd : ReactiveProperty<_> = replaceWithToAdd
    member __.AddReplacementCommand = addReplacementCommand
    member __.Replacements : ObservableCollection<_> = replacements

    member __.NewNameToAdd : ReactiveProperty<string> = newNameToAdd
    member __.ClearNewNameToAdd() = clearNewNameToAdd ()
    member __.ClearNewNameToAddCommand = clearNewNameToAddCommand
    member __.AddName(name : string) = addName name
    member __.AddNameCommand = addNameCommand

    member __.Names : ObservableCollection<NameViewModel> = names

    member __.ResetNameSelectionCommand : ReactiveCommand = resetNameSelectionCommand
    member __.SearchForTextCommand = searchForTextCommand
    member __.SearchForNameCommand = searchForNameCommand
    member __.DeleteNameCommand = deleteNameCommand

    member __.EditingFeatureInstances : ObservableCollection<NewFeatureInstanceViewModel> = editingFeatureInstances
    member __.RemoveFeatureInstanceRowCommand = removeFeatureInstanceRowCommand
    member __.AddFeatureInstanceRowCommand = addFeatureInstanceRowCommand
    member __.ClearSelectedFeatureCommand = clearSelectedFeatureCommand
    member __.AddFeatureInstanceRow() =
        NewFeatureInstanceViewModel() |> this.EditingFeatureInstances.Add
    member __.ExpandAllFeaturesCommand = expandAllFeaturesCommand
    member __.CollapseAllFeaturesCommand = collapseAllFeaturesCommand

    member __.EditingFeatureName : ReactiveProperty<string> = editingFeatureName
    member __.EditingFeatureCode : ReactiveProperty<string> = editingFeatureCode
    member __.EditingFeatureToInclude : ReactiveProperty<string> = editingFeatureToInclude

    member __.ConfirmEditingFeatureCommand = confirmEditingFeatureCommand

    member __.SelectedFeature : ReactiveProperty<FeatureViewModel> = selectedFeature

    member __.SelectedFeatureInstances =
        this.FeatureInstances
        |> Seq.filter (fun vm -> vm.IsSelected)
        |> Seq.map (fun vm -> vm.FeatureCode + vm.InstanceCode)

    member __.Features : ReactiveList<FeatureViewModel> = features

    member __.FeatureInstances : ReactiveList<FeatureInstanceViewModel> = featureInstances

    member __.IsFeatureDragDropReorderingEnabled : ReactiveProperty<bool> = isFeatureDragDropReorderingEnabled

    member __.ResultingFilePath : ReactiveProperty<string> = resultingFilePath

    member __.ApplyCommand = applyCommand
