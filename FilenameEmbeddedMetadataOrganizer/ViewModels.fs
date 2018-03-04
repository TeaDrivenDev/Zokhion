namespace FilenameEmbeddedMetadataOrganizer.ViewModels

open System
open System.Collections.ObjectModel
open System.Diagnostics
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open System.Windows
open System.Windows.Input
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

    let mutable baseDirectory = Unchecked.defaultof<string>
    let mutable selectedDirectory = Unchecked.defaultof<string>
    let mutable selectedFile = Unchecked.defaultof<FileInfo>

    let mutable originalFileName = Unchecked.defaultof<string>
    let mutable newFileName = Unchecked.defaultof<string>

    let mutable treatParenthesizedPartAsNames = true
    let mutable fixupNamesInMainPart = Unchecked.defaultof<bool>
    let mutable replaceUnderscores = true
    let mutable detectNamesInMainAndNamesParts = Unchecked.defaultof<bool>

    let reverseString (s : string) = s |> Seq.rev |> String.Concat
    let directories = ObservableCollection()
    let files = ObservableCollection()

    let destinationDirectories = ObservableCollection()
    let mutable selectedDestinationDirectory = Unchecked.defaultof<DirectoryInfo>

    let mutable resultingFilePath = Unchecked.defaultof<string>

    let mutable openCommand = Unchecked.defaultof<ReactiveCommand>

    let names = ObservableCollection()
    let mutable selectedNames = Unchecked.defaultof<string>

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

    let updateNewNameFromNamesSelection () =
        this.SelectedNames.Split([| "||" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> Some
        |> updateNewName

    let updateResultingFilePath () =
        if not <| isNull this.SelectedDestinationDirectory
        then
            this.ResultingFilePath <-
                Path.Combine(this.SelectedDestinationDirectory.FullName,
                             this.NewFileName + Path.GetExtension(this.SelectedFile.Name))

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

        this.ObservableForProperty(toLinq <@ fun vm -> vm.BaseDirectory @>)
            .Throttle(TimeSpan.FromSeconds 1., RxApp.MainThreadScheduler)
            .SubscribeOnDispatcher()
            .Subscribe(fun x ->
                if Directory.Exists x.Value
                then
                    Directory.GetDirectories x.Value
                    |> Seq.map Path.GetFileName
                    |> Seq.filter (fun s -> s.StartsWith "_" || s.StartsWith "b")
                    |> Seq.sort
                    |> Seq.iter directories.Add

                    loadSettings x.Value)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SelectedDirectory @>)
            .SubscribeOnDispatcher()
            .Subscribe(fun dir ->
                files.Clear()

                Directory.GetFiles (Path.Combine(this.BaseDirectory, dir.Value))
                |> Seq.map FileInfo
                |> Seq.sortBy (fun fi -> fi.Name)
                |> Seq.iter files.Add)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SelectedFile @>)
            .SubscribeOnDispatcher()
            .Subscribe(fun (fi : IObservedChange<_, FileInfo>) ->
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
            .SubscribeOnDispatcher()
            .Subscribe(fun name ->
                this.NewFileName <- name.Value
                updateNewName None

                updateDestinationDirectories this.SelectedFile.FullName)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.TreatParenthesizedPartAsNames @>)
            .Subscribe(fun _ -> updateNewName None)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.FixupNamesInMainPart @>)
            .Subscribe(fun _ -> updateNewName None)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.ReplaceUnderscores @>)
            .Subscribe(fun _ -> updateNewName None)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.DetectNamesInMainAndNamesParts @>)
            .Subscribe(fun _ -> updateNewName None)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.NewFileName @>)
            .Subscribe(fun _ -> updateResultingFilePath ())
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SelectedDestinationDirectory @>)
            .Subscribe(fun _ -> updateResultingFilePath ())
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SelectedNames @>)
            .Subscribe(fun _ -> updateNewNameFromNamesSelection ())
        |> ignore

    member __.ShutDown () =
        let names =
            this.Names
            |> Seq.filter (fun vm -> not vm.IsNew)
            |> Seq.map (fun vm -> vm.Name)
            |> Seq.sort

        if not <| Seq.isEmpty names
        then
            let namesFilePath = Path.Combine(this.BaseDirectory, ".names")

            File.WriteAllLines(namesFilePath, names)

    member __.BaseDirectory
        with get () = baseDirectory
        and set value = this.RaiseAndSetIfChanged(&baseDirectory, value, nameof <@ __.BaseDirectory @>) |> ignore

    member __.Directories = directories
    member __.Files = files

    member __.SelectedDirectory
        with get () = selectedDirectory
        and set value = this.RaiseAndSetIfChanged(&selectedDirectory, value, nameof <@ __.SelectedDirectory @>) |> ignore

    member __.SelectedFile
        with get () : FileInfo = selectedFile
        and set value = this.RaiseAndSetIfChanged(&selectedFile, value, nameof <@ __.SelectedFile @>) |> ignore

    member __.OriginalFileName
        with get () = originalFileName
        and set value = this.RaiseAndSetIfChanged(&originalFileName, value, nameof <@ __.OriginalFileName @>) |> ignore

    member __.NewFileName
        with get () = newFileName
        and set value = this.RaiseAndSetIfChanged(&newFileName, value, nameof <@ __.NewFileName @>) |> ignore

    member __.OpenCommand = openCommand :> ICommand

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

    member __.DestinationDirectories = destinationDirectories

    member __.SelectedDestinationDirectory
        with get ()  : DirectoryInfo= selectedDestinationDirectory
        and set value = this.RaiseAndSetIfChanged(&selectedDestinationDirectory, value, nameof <@ __.SelectedDestinationDirectory @>) |> ignore

    member __.ResultingFilePath
        with get () = resultingFilePath
        and set value = this.RaiseAndSetIfChanged(&resultingFilePath, value, nameof <@ __.ResultingFilePath @>) |> ignore

    member __.Names : ObservableCollection<NameViewModel> = names

    member __.SelectedNames
        with get () : string = selectedNames
        and set value = this.RaiseAndSetIfChanged(&selectedNames, value, nameof <@ __.SelectedNames @>) |> ignore
