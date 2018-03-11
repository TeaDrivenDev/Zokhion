namespace FilenameEmbeddedMetadataOrganizer.ViewModels

open System
open System.Collections.ObjectModel
open System.Diagnostics
open System.IO
open System.Reactive.Concurrency
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

type FeatureViewModel(name : string, code : string) =
    inherit ReactiveObject()

    let instances = ReactiveList<FeatureInstanceViewModel>()

    member __.FeatureName = name

    member __.FeatureCode = code

    member __.Instances = instances

and FeatureInstanceViewModel(featureName : string, featureCode : string, instanceName : string, instanceCode : string) =
    inherit FeatureViewModel(featureName, featureCode)

    let mutable isSelected = Unchecked.defaultof<bool>

    member __.InstanceName = instanceName

    member __.InstanceCode = instanceCode

    member __.IsSelected
        with get () = isSelected
        and set value = __.RaiseAndSetIfChanged(&isSelected, value, nameof <@ __.IsSelected @>) |> ignore

[<AllowNullLiteral>]
type NameViewModel() as this =
    inherit ReactiveObject()

    let mutable name = Unchecked.defaultof<string>
    let mutable isSelected = Unchecked.defaultof<bool>
    let mutable isNew = Unchecked.defaultof<bool>

    let clearNewFlagCommand =
        ReactiveCommand.Create(fun () -> this.IsNew <- false)

    member __.Name
        with get () = name
        and set value = this.RaiseAndSetIfChanged(&name, value, nameof <@ __.Name @>) |> ignore

    member __.IsSelected
        with get () = isSelected
        and set value = this.RaiseAndSetIfChanged(&isSelected, value, nameof <@ __.IsSelected @>) |> ignore

    member __.IsNew
        with get () = isNew
        and set value = this.RaiseAndSetIfChanged(&isNew, value, nameof <@ __.IsNew @>) |> ignore

    member __.ClearNewFlagCommand = clearNewFlagCommand :> ICommand

type MainWindowViewModel() as this =
    inherit ReactiveObject()

    let mutable baseDirectory = ""
    let mutable selectedDirectory = ""
    let directories = ObservableCollection()

    let mutable searchString = ""
    let mutable searchFromBaseDirectory = false
    let mutable selectedFile = Unchecked.defaultof<FileInfo>
    let files = ObservableCollection()

    let mutable originalFileName = ""
    let mutable newFileName = ""

    let mutable treatParenthesizedPartAsNames = true
    let mutable fixupNamesInMainPart = false
    let mutable replaceUnderscores = true
    let mutable detectNamesInMainAndNamesParts = false

    let mutable openCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable openExplorerCommand = Unchecked.defaultof<ReactiveCommand>

    let mutable selectedDestinationDirectory = Unchecked.defaultof<DirectoryInfo>
    let destinationDirectories = ObservableCollection()

    let mutable newNameToAdd = Unchecked.defaultof<string>
    let mutable addNameCommand = Unchecked.defaultof<ReactiveCommand>
    let names = ReactiveList(ChangeTrackingEnabled = true)

    let mutable addFeatureRoot = Unchecked.defaultof<bool>
    let mutable featureToAdd = Unchecked.defaultof<string>
    let mutable featureCodeToAdd = Unchecked.defaultof<string>
    let mutable addFeatureCommand = Unchecked.defaultof<ReactiveCommand>
    let mutable selectedFeature = Unchecked.defaultof<FeatureViewModel>
    let features = ReactiveList()
    let featureInstances = ReactiveList(ChangeTrackingEnabled = true)

    let mutable resultingFilePath = Unchecked.defaultof<string>

    let updateDestinationDirectories (currentFilePath : string) =
        let startsWith part (s : string) = s.StartsWith part

        let currentFileDirectory = Path.GetDirectoryName currentFilePath

        destinationDirectories.Clear()

        currentFileDirectory ::
        (Directory.GetDirectories this.BaseDirectory
        |> Array.filter (Path.GetFileName >> startsWith "_")
        |> Array.sort
        |> Array.toList)
        |> List.distinct
        |> List.map DirectoryInfo
        |> List.iter destinationDirectories.Add

        this.SelectedDestinationDirectory <-
            destinationDirectories
            |> Seq.find (fun (d : DirectoryInfo) -> d.FullName = currentFileDirectory)

    let updateNamesList detectedNames =
        (detectedNames, this.Names)
        ||> fullOuterJoin id (fun vm -> vm.Name)
        |> Seq.iter (function
            | LeftOnly vm -> vm.IsSelected <- false
            | RightOnly name ->
                NameViewModel(Name = name, IsSelected = true, IsNew = true)
                |> names.Add
            | JoinMatch (vm, name) -> vm.IsSelected <- true)

    let updateNewName selectedNames =
        let allNames =
            this.Names |> Seq.map (fun vm -> vm.Name) |> Seq.toList

        let parameters =
            {
                SelectedFeatures = None
                AllNames = allNames
                SelectedNames = selectedNames
                TreatParenthesizedPartAsNames = this.TreatParenthesizedPartAsNames
                DetectNamesInMainAndNamesParts = false
                FixupNamesInMainPart = this.FixupNamesInMainPart
                ReplaceUnderscores = this.ReplaceUnderscores
                Replacements = []
            }

        let result = rename parameters this.NewFileName
        this.NewFileName <- result.NewFileName

        updateNamesList result.DetectedNames

    let updateResultingFilePath () =
        if not <| isNull this.SelectedDestinationDirectory
        then
            this.SelectedFile
            |> Option.ofObj
            |> Option.iter (fun selectedFile ->
                this.ResultingFilePath <-
                    Path.Combine(this.SelectedDestinationDirectory.FullName,
                                 this.NewFileName + Path.GetExtension(selectedFile.Name)))

    let loadSettings baseDirectory =
        let namesFilePath = Path.Combine(baseDirectory, ".names")

        if File.Exists namesFilePath
        then
            let names = File.ReadAllLines namesFilePath

            names
            |> Seq.iter (fun name -> NameViewModel(Name = name) |> this.Names.Add)

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
                    NameViewModel(Name = name) |> this.Names.Add)

        addFeatureCommand <-
            ReactiveCommand.Create(fun () ->
                if not <| String.IsNullOrWhiteSpace this.FeatureToAdd
                    && not <| String.IsNullOrWhiteSpace this.FeatureCodeToAdd
                then
                    if this.AddFeatureRoot
                    then
                        FeatureViewModel(this.FeatureToAdd, this.FeatureCodeToAdd)
                        |> features.Add
                    else
                        match this.SelectedFeature with
                        | :? FeatureInstanceViewModel -> ()
                        | :? FeatureViewModel as feature ->
                            let instance =
                                FeatureInstanceViewModel(feature.FeatureName, feature.FeatureCode, this.FeatureToAdd, this.FeatureCodeToAdd)

                            feature.Instances.Add instance
                            featureInstances.Add instance
                        | _ -> ())

        this.Names.ItemChanged
        |> Observable.filter (fun change ->
            change.PropertyName = nameof <@ any<NameViewModel>.IsSelected @>)
        |> Observable.subscribe (fun _ ->
            this.Names
            |> Seq.filter (fun n -> n.IsSelected)
            |> Seq.map (fun n-> n.Name)
            |> Seq.toList
            |> Some
            |> updateNewName)
        |> ignore

        this.FeatureInstances.ItemChanged
        |> Observable.filter (fun change -> change.PropertyName = nameof <@ any<FeatureInstanceViewModel>.IsSelected @>)
        |> Observable.subscribe (fun _ -> this.RaisePropertyChanged(nameof <@ this.SelectedFeatureInstances @>))
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.BaseDirectory @>)
        |> Observable.throttleOn RxApp.MainThreadScheduler (TimeSpan.FromSeconds 1.)
        |> Observable.subscribe (fun x ->
            if Directory.Exists x.Value
            then
                directories.Clear()

                Directory.GetDirectories x.Value
                |> Seq.map Path.GetFileName
                //|> Seq.filter (fun s -> s.StartsWith "_" || s.StartsWith "b")
                |> Seq.sort
                |> Seq.iter directories.Add

                loadSettings x.Value)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SelectedDirectory @>)
        |> Observable.subscribe (fun dir ->
            files.Clear()

            if not <| String.IsNullOrWhiteSpace dir.Value
            then
                Directory.GetFiles(Path.Combine(this.BaseDirectory, dir.Value), "*", SearchOption.AllDirectories)
                |> Seq.map FileInfo
                |> Seq.sortBy (fun fi -> fi.Name)
                |> Seq.iter files.Add)
        |> ignore

        let getFiles directory part =
            Directory.GetFiles(Path.Combine(this.BaseDirectory, directory), sprintf "*%s*" part, SearchOption.AllDirectories)
            |> Seq.map FileInfo
            |> Seq.filter (fun fi -> (fi.Name |> Path.GetFileNameWithoutExtension |> toUpper).Contains(toUpper part))
            |> Seq.sortBy (fun fi -> fi.Name)
            |> Observable.ofSeq

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SearchString @>)
        |> Observable.throttle (TimeSpan.FromMilliseconds 500.)
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.map (fun change -> change.Value)
        |> Observable.distinctUntilChanged
        |> Observable.iter (fun _ ->  files.Clear())
        |> Observable.observeOn Scheduler.Default
        |> Observable.map (fun change -> getFiles this.SelectedDirectory change)
        |> Observable.switch
        |> Observable.observeOn RxApp.MainThreadScheduler
        |> Observable.subscribe files.Add
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SelectedFile @>)
        |> Observable.subscribe (fun (fi : IObservedChange<_, FileInfo>) ->
            if not <| isNull fi.Value
            then
                this.Names
                |> Seq.filter (fun vm -> vm.IsNew)
                |> Seq.toList
                |> List.iter (this.Names.Remove >> ignore)

                this.OriginalFileName <-
                    string fi.Value.Name |> Path.GetFileNameWithoutExtension)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.OriginalFileName @>)
        |> Observable.subscribe (fun name ->
            this.NewFileName <- name.Value
            updateNewName None

            updateDestinationDirectories this.SelectedFile.FullName)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.NewFileName @>)
        |> Observable.subscribe (fun _ -> updateResultingFilePath ())
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.TreatParenthesizedPartAsNames @>)
        |> Observable.subscribe (fun _ -> updateNewName None)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.FixupNamesInMainPart @>)
        |> Observable.subscribe (fun _ -> updateNewName None)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.ReplaceUnderscores @>)
        |> Observable.subscribe (fun _ -> updateNewName None)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.DetectNamesInMainAndNamesParts @>)
        |> Observable.subscribe (fun _ -> updateNewName None)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SelectedDestinationDirectory @>)
        |> Observable.subscribe (fun _ -> updateResultingFilePath ())
        |> ignore

    member __.ShutDown () =
        if Directory.Exists this.BaseDirectory
        then
            let names =
                this.Names
                |> Seq.filter (fun vm -> not vm.IsNew)
                |> Seq.map (fun vm -> vm.Name)
                |> Seq.distinct
                |> Seq.sort

            if not <| Seq.isEmpty names
            then
                let namesFilePath = Path.Combine(this.BaseDirectory, ".names")

                File.WriteAllLines(namesFilePath, names)

    member __.BaseDirectory
        with get () = baseDirectory
        and set value = this.RaiseAndSetIfChanged(&baseDirectory, value, nameof <@ __.BaseDirectory @>) |> ignore

    member __.SelectedDirectory
        with get () = selectedDirectory
        and set value = this.RaiseAndSetIfChanged(&selectedDirectory, value, nameof <@ __.SelectedDirectory @>) |> ignore

    member __.Directories = directories

    member __.SearchString
        with get () = searchString
        and set value = __.RaiseAndSetIfChanged(&searchString, value, nameof <@ __.SearchString @>) |> ignore

    member __.SearchFromBaseDirectory
        with get () = searchFromBaseDirectory
        and set value = __.RaiseAndSetIfChanged(&searchFromBaseDirectory, value, nameof <@ __.SearchFromBaseDirectory @>) |> ignore

    member __.SelectedFile
        with get () : FileInfo = selectedFile
        and set value = this.RaiseAndSetIfChanged(&selectedFile, value, nameof <@ __.SelectedFile @>) |> ignore

    member __.Files = files

    member __.OriginalFileName
        with get () = originalFileName
        and set value = this.RaiseAndSetIfChanged(&originalFileName, value, nameof <@ __.OriginalFileName @>) |> ignore

    member __.NewFileName
        with get () = newFileName
        and set value = this.RaiseAndSetIfChanged(&newFileName, value, nameof <@ __.NewFileName @>) |> ignore

    member __.TreatParenthesizedPartAsNames
        with get () = treatParenthesizedPartAsNames
        and set value = this.RaiseAndSetIfChanged(&treatParenthesizedPartAsNames, value, nameof <@ __.TreatParenthesizedPartAsNames @>) |> ignore

    member __.FixupNamesInMainPart
        with get () = fixupNamesInMainPart
        and set value = this.RaiseAndSetIfChanged(&fixupNamesInMainPart, value, nameof <@ __.FixupNamesInMainPart @>) |> ignore

    member __.ReplaceUnderscores
        with get () = replaceUnderscores
        and set value = this.RaiseAndSetIfChanged(&replaceUnderscores, value, nameof <@ __.ReplaceUnderscores @>) |> ignore

    member __.DetectNamesInMainAndNamesParts
        with get () = detectNamesInMainAndNamesParts
        and set value = this.RaiseAndSetIfChanged(&detectNamesInMainAndNamesParts, value, nameof <@ __.DetectNamesInMainAndNamesParts @>) |> ignore

    member __.OpenCommand = openCommand :> ICommand
    member __.OpenExplorerCommand = openExplorerCommand :> ICommand

    member __.SelectedDestinationDirectory
        with get ()  : DirectoryInfo= selectedDestinationDirectory
        and set value = this.RaiseAndSetIfChanged(&selectedDestinationDirectory, value, nameof <@ __.SelectedDestinationDirectory @>) |> ignore

    member __.DestinationDirectories = destinationDirectories

    member __.NewNameToAdd
        with get () = newNameToAdd
        and set value = this.RaiseAndSetIfChanged(&newNameToAdd, value, nameof <@ __.NewNameToAdd @>) |> ignore

    member __.AddNameCommand = addNameCommand :> ICommand

    member __.Names : ReactiveList<NameViewModel> = names

    member __.AddFeatureRoot
        with get () = addFeatureRoot
        and set value = __.RaiseAndSetIfChanged(&addFeatureRoot, value, nameof <@ __.AddFeatureRoot @>) |> ignore

    member __.FeatureToAdd
        with get () = featureToAdd
        and set value = __.RaiseAndSetIfChanged(&featureToAdd, value, nameof <@ __.FeatureToAdd @>) |> ignore

    member __.FeatureCodeToAdd
        with get () = featureCodeToAdd
        and set value = __.RaiseAndSetIfChanged(&featureCodeToAdd, value, nameof <@ __.FeatureCodeToAdd @>) |> ignore

    member __.AddFeatureCommand = addFeatureCommand

    member __.SelectedFeature
        with get () : FeatureViewModel = selectedFeature
        and set value = __.RaiseAndSetIfChanged(&selectedFeature, value, nameof <@ __.SelectedFeature @>) |> ignore

    member __.SelectedFeatureInstances =
        this.FeatureInstances
            |> Seq.filter (fun vm -> vm.IsSelected)
            |> Seq.map (fun vm -> vm.FeatureCode + vm.InstanceCode)

    member __.Features : ReactiveList<FeatureViewModel> = features

    member __.FeatureInstances : ReactiveList<FeatureInstanceViewModel> = featureInstances

    member __.ResultingFilePath
        with get () = resultingFilePath
        and set value = this.RaiseAndSetIfChanged(&resultingFilePath, value, nameof <@ __.ResultingFilePath @>) |> ignore
