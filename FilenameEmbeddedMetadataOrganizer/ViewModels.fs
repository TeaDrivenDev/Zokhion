namespace FilenameEmbeddedMetadataOrganizer.ViewModels

open System
open System.Collections.ObjectModel
open System.Diagnostics
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Subjects
open System.Windows
open System.Windows.Input

open FSharp.Control.Reactive

open ReactiveUI

open FilenameEmbeddedMetadataOrganizer

[<AutoOpen>]
module Utility =
    open System.Linq.Expressions
    open FSharp.Quotations

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

type ReactiveProperty<'a>(initialValue : 'a) =
    inherit ReactiveObject()

    let mutable xvalue = initialValue
    let source = new Subject<'a>()

    member __.Value
        with get () = xvalue
        and set value =
            __.RaiseAndSetIfChanged(&xvalue, value, nameof <@ __.Value @>) |> ignore
            source.OnNext value

    new () = ReactiveProperty(Unchecked.defaultof<'a>)

    interface IObservable<'a>
        with member __.Subscribe (observer : IObserver<'a>) = source.Subscribe observer

type FeatureViewModel(name : string, code : string) =
    inherit ReactiveObject()

    member __.FeatureName = name

    member __.FeatureCode = code

    member val Instances = ReactiveList<FeatureInstanceViewModel>()

and FeatureInstanceViewModel(featureName : string, featureCode : string, instanceName : string, instanceCode : string) =
    inherit FeatureViewModel(featureName, featureCode)

    member __.InstanceName = instanceName

    member __.InstanceCode = instanceCode

    member val IsSelected = ReactiveProperty false

[<AllowNullLiteral>]
type NameViewModel(name : string, isSelected : bool, isNew : bool) as this =
    inherit ReactiveObject()

    member __.Name = name

    member val IsSelected = ReactiveProperty isSelected

    member val IsNew : ReactiveProperty<bool> = ReactiveProperty isNew

    member __.ClearNewFlagCommand = ReactiveCommand.Create(fun () -> this.IsNew.Value <- false)

type MainWindowViewModel() as this =
    inherit ReactiveObject()

    let baseDirectory = ReactiveProperty ""
    let selectedDirectory = ReactiveProperty ""
    let directories = ObservableCollection()

    let searchString = ReactiveProperty<string>()
    let searchFromBaseDirectory = ReactiveProperty false
    let selectedFile = ReactiveProperty<FileInfo>()
    let files = ObservableCollection()

    let originalFileName = ReactiveProperty ""
    let newFileName = ReactiveProperty ""

    let treatParenthesizedPartAsNames = ReactiveProperty true
    let fixupNamesInMainPart = ReactiveProperty false
    let replaceUnderscores = ReactiveProperty true
    let detectNamesInMainAndNamesParts = ReactiveProperty false

    let mutable openCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable openExplorerCommand = Unchecked.defaultof<ReactiveCommand>

    let selectedDestinationDirectory = ReactiveProperty<DirectoryInfo>()
    let destinationDirectories = ObservableCollection()

    let newNameToAdd = ReactiveProperty ""
    let mutable addNameCommand = Unchecked.defaultof<ReactiveCommand>
    let names = ReactiveList(ChangeTrackingEnabled = true)

    let addFeatureRoot = ReactiveProperty false
    let featureToAdd = ReactiveProperty<string>()
    let featureCodeToAdd = ReactiveProperty<string>()
    let mutable addFeatureCommand = Unchecked.defaultof<ReactiveCommand>
    let selectedFeature = ReactiveProperty<FeatureViewModel>()
    let features = ReactiveList()
    let featureInstances = ReactiveList(ChangeTrackingEnabled = true)

    let resultingFilePath = ReactiveProperty ""

    let getFiles directory part =
        Directory.GetFiles(Path.Combine(this.BaseDirectory.Value, directory), sprintf "*%s*" part, SearchOption.AllDirectories)
        |> Seq.map FileInfo
        |> Seq.filter (fun fi -> (fi.Name |> Path.GetFileNameWithoutExtension |> toUpper).Contains(toUpper part))
        |> Seq.sortBy (fun fi -> fi.Name)
        |> Observable.ofSeq

    let updateDestinationDirectories (currentFilePath : string) =
        let startsWith part (s : string) = s.StartsWith part

        let currentFileDirectory = Path.GetDirectoryName currentFilePath

        destinationDirectories.Clear()

        currentFileDirectory ::
        (Directory.GetDirectories this.BaseDirectory.Value
        |> Array.filter (Path.GetFileName >> startsWith "_")
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
        ||> fullOuterJoin id (fun vm -> vm.Name)
        |> Seq.iter (function
            | LeftOnly vm -> vm.IsSelected.Value <- false
            | RightOnly name ->
                NameViewModel(name, true, true)
                |> names.Add
            | JoinMatch (vm, name) -> vm.IsSelected.Value <- true)

    let updateNewName selectedNames =
        let allNames =
            this.Names |> Seq.map (fun vm -> vm.Name) |> Seq.toList

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = selectedNames
                TreatParenthesizedPartAsNames = this.TreatParenthesizedPartAsNames.Value
                DetectNamesInMainAndNamesParts = this.DetectNamesInMainAndNamesParts.Value
                FixupNamesInMainPart = this.FixupNamesInMainPart.Value
                ReplaceUnderscores = this.ReplaceUnderscores.Value
                Replacements = []
            }

        let result = rename parameters this.NewFileName.Value
        this.NewFileName.Value <- result.NewFileName

        updateNamesList result.DetectedNames

    let updateResultingFilePath () =
        if not <| isNull this.SelectedDestinationDirectory.Value
        then
            this.SelectedFile.Value
            |> Option.ofObj
            |> Option.iter (fun selectedFile ->
                this.ResultingFilePath.Value <-
                    Path.Combine(this.SelectedDestinationDirectory.Value.FullName,
                                 this.NewFileName.Value + Path.GetExtension(selectedFile.Name)))

    let loadSettings baseDirectory =
        let namesFilePath = Path.Combine(baseDirectory, ".names")

        if File.Exists namesFilePath
        then
            let names = File.ReadAllLines namesFilePath

            names
            |> Seq.iter (fun name -> NameViewModel(name, false, false) |> this.Names.Add)

    do
        RxApp.MainThreadScheduler <- DispatcherScheduler(Application.Current.Dispatcher)

        openCommand <-
            ReactiveCommand.Create(fun (fi : FileInfo) -> Process.Start fi.FullName |> ignore)

        openExplorerCommand <-
            ReactiveCommand.Create(fun (fi: FileInfo) ->
                fi.FullName
                |> sprintf "/select, \"%s\""
                |> asSnd "explorer.exe"
                |> Process.Start
                |> ignore)

        addNameCommand <-
            ReactiveCommand.Create(fun name ->
                if not <| String.IsNullOrWhiteSpace name
                then
                    NameViewModel(name, false, false) |> this.Names.Add)

        addFeatureCommand <-
            ReactiveCommand.Create(fun () ->
                if not <| String.IsNullOrWhiteSpace this.FeatureToAdd.Value
                    && not <| String.IsNullOrWhiteSpace this.FeatureCodeToAdd.Value
                then
                    if this.AddFeatureRoot.Value
                    then
                        FeatureViewModel(this.FeatureToAdd.Value, this.FeatureCodeToAdd.Value)
                        |> features.Add
                    else
                        match this.SelectedFeature.Value with
                        | :? FeatureInstanceViewModel -> ()
                        | :? FeatureViewModel as feature ->
                            let instance =
                                FeatureInstanceViewModel(feature.FeatureName, feature.FeatureCode, this.FeatureToAdd.Value, this.FeatureCodeToAdd.Value)

                            feature.Instances.Add instance
                            featureInstances.Add instance
                        | _ -> ())

        this.Names.ItemChanged
        |> Observable.filter (fun change ->
            change.PropertyName = nameof <@ any<NameViewModel>.IsSelected @>)
        |> Observable.subscribe (fun _ ->
            this.Names
            |> Seq.filter (fun n -> n.IsSelected.Value)
            |> Seq.map (fun n-> n.Name)
            |> Seq.toList
            |> Some
            |> updateNewName)
        |> ignore

        this.FeatureInstances.ItemChanged
        |> Observable.filter (fun change -> change.PropertyName = nameof <@ any<FeatureInstanceViewModel>.IsSelected @>)
        |> Observable.subscribe (fun _ -> this.RaisePropertyChanged(nameof <@ this.SelectedFeatureInstances @>))
        |> ignore

        this.BaseDirectory
        |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromSeconds 1.)
        |> Observable.subscribe (fun x ->
            if Directory.Exists x
            then
                directories.Clear()

                Directory.GetDirectories x
                |> Seq.map Path.GetFileName
                //|> Seq.filter (fun s -> s.StartsWith "_" || s.StartsWith "b")
                |> Seq.sort
                |> Seq.iter directories.Add

                loadSettings x)
        |> ignore

        this.SelectedDirectory
        |> Observable.subscribe (fun dir ->
            files.Clear()

            if not <| String.IsNullOrWhiteSpace dir
            then
                Directory.GetFiles(Path.Combine(this.BaseDirectory.Value, dir), "*", SearchOption.AllDirectories)
                |> Seq.map FileInfo
                |> Seq.sortBy (fun fi -> fi.Name)
                |> Seq.iter files.Add)
        |> ignore

        this.SearchFromBaseDirectory
        |> Observable.startWith [ false ]
        |> Observable.combineLatest this.SearchString
        |> Observable.throttle (TimeSpan.FromMilliseconds 200.)
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.map (fun (searchString, flag) -> searchString, flag)
        |> Observable.distinctUntilChanged
        |> Observable.iter (fun _ ->  files.Clear())
        |> Observable.observeOn Scheduler.Default
        |> Observable.map (fun (searchString, flag) ->
            getFiles (if flag then "" else this.SelectedDirectory.Value) searchString)
        |> Observable.switch
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe files.Add
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

        this.OriginalFileName
        |> Observable.subscribe (fun name ->
            this.NewFileName.Value <- name
            updateNewName None

            updateDestinationDirectories this.SelectedFile.Value.FullName)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.NewFileName @>)
        |> Observable.subscribe (fun _ -> updateResultingFilePath ())
        |> ignore

        [
            this.TreatParenthesizedPartAsNames
            this.FixupNamesInMainPart
            this.ReplaceUnderscores
            this.DetectNamesInMainAndNamesParts
        ]
        |> Seq.cast<IObservable<bool>>
        |> Observable.mergeSeq
        |> Observable.subscribe (fun _ -> updateNewName None)
        |> ignore

        this.NewFileName
        |> Observable.combineLatest this.SelectedDestinationDirectory
        |> Observable.subscribe (fun _ -> updateResultingFilePath ())
        |> ignore

    member __.ShutDown () =
        if Directory.Exists this.BaseDirectory.Value
        then
            let names =
                this.Names
                |> Seq.filter (fun vm -> not vm.IsNew.Value)
                |> Seq.map (fun vm -> vm.Name)
                |> Seq.distinct
                |> Seq.sort

            if not <| Seq.isEmpty names
            then
                let namesFilePath = Path.Combine(this.BaseDirectory.Value, ".names")

                File.WriteAllLines(namesFilePath, names)

    member __.BaseDirectory : ReactiveProperty<string> = baseDirectory

    member __.SelectedDirectory : ReactiveProperty<string> = selectedDirectory

    member __.Directories = directories

    member __.SearchString = searchString

    member __.SearchFromBaseDirectory = searchFromBaseDirectory

    member __.SelectedFile : ReactiveProperty<FileInfo> = selectedFile

    member __.Files = files

    member __.OriginalFileName : ReactiveProperty<string> = originalFileName

    member __.NewFileName : ReactiveProperty<string> = newFileName

    member __.TreatParenthesizedPartAsNames : ReactiveProperty<bool> = treatParenthesizedPartAsNames

    member __.FixupNamesInMainPart : ReactiveProperty<bool> = fixupNamesInMainPart

    member __.ReplaceUnderscores : ReactiveProperty<bool> = replaceUnderscores

    member __.DetectNamesInMainAndNamesParts : ReactiveProperty<bool> = detectNamesInMainAndNamesParts

    member __.OpenCommand = openCommand :> ICommand
    member __.OpenExplorerCommand = openExplorerCommand :> ICommand

    member __.SelectedDestinationDirectory : ReactiveProperty<DirectoryInfo> = selectedDestinationDirectory

    member __.DestinationDirectories = destinationDirectories

    member __.NewNameToAdd : ReactiveProperty<string> = newNameToAdd

    member __.AddNameCommand = addNameCommand :> ICommand

    member __.Names : ReactiveList<NameViewModel> = names

    member __.AddFeatureRoot : ReactiveProperty<bool> = addFeatureRoot

    member __.FeatureToAdd : ReactiveProperty<string> = featureToAdd

    member __.FeatureCodeToAdd : ReactiveProperty<string> = featureCodeToAdd

    member __.AddFeatureCommand = addFeatureCommand

    member __.SelectedFeature : ReactiveProperty<FeatureViewModel> = selectedFeature

    member __.SelectedFeatureInstances =
        this.FeatureInstances
        |> Seq.filter (fun vm -> vm.IsSelected.Value)
        |> Seq.map (fun vm -> vm.FeatureCode + vm.InstanceCode)

    member __.Features : ReactiveList<FeatureViewModel> = features

    member __.FeatureInstances : ReactiveList<FeatureInstanceViewModel> = featureInstances

    member __.ResultingFilePath : ReactiveProperty<string> = resultingFilePath
