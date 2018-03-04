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

type MainWindowViewModel() as this =
    inherit ReactiveObject()

    let mutable baseDirectory = Unchecked.defaultof<string>
    let mutable selectedDirectory = Unchecked.defaultof<string>
    let mutable selectedFile = Unchecked.defaultof<FileInfo>

    let mutable originalFileName = Unchecked.defaultof<string>
    let mutable newFileName = Unchecked.defaultof<string>

    let mutable treatParenthesizedPartAsNames = Unchecked.defaultof<bool>
    let mutable fixupNamesInMainPart = Unchecked.defaultof<bool>

    let reverseString (s : string) = s |> Seq.rev |> String.Concat
    let directories = ObservableCollection()
    let files = ObservableCollection()

    let destinationDirectories = ObservableCollection()
    let mutable selectedDestinationDirectory = Unchecked.defaultof<DirectoryInfo>

    let mutable resultingFilePath = Unchecked.defaultof<string>

    let mutable openCommand = Unchecked.defaultof<ReactiveCommand>

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

    let updateNewName () =
        let parameters =
            {
                SelectedFeatures = None
                AllNames = []
                SelectedNames = None
                TreatParenthesizedPartAsNames = this.TreatParenthesizedPartAsNames
                FixupNamesInMainPart = this.FixupNamesInMainPart
                Replacements = []
            }

        this.NewFileName <- (rename parameters this.OriginalFileName).NewFileName

    let updateResultingFilePath () =
        if not <| isNull this.SelectedDestinationDirectory
        then
            this.ResultingFilePath <-
                Path.Combine(this.SelectedDestinationDirectory.FullName,
                             this.NewFileName + Path.GetExtension(this.SelectedFile.Name))

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
                    |> Seq.iter directories.Add)
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
                    this.OriginalFileName <-string fi.Value.Name |> Path.GetFileNameWithoutExtension)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.OriginalFileName @>)
            .SubscribeOnDispatcher()
            .Subscribe(fun _ ->
                this.TreatParenthesizedPartAsNames <- true
                this.FixupNamesInMainPart <- true

                updateNewName ()

                updateDestinationDirectories this.SelectedFile.FullName)
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.TreatParenthesizedPartAsNames @>)
            .SubscribeOnDispatcher()
            .Subscribe(fun _ -> updateNewName ())
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.FixupNamesInMainPart @>)
            .SubscribeOnDispatcher()
            .Subscribe(fun _ -> updateNewName ())
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.NewFileName @>)
            .Subscribe(fun _ -> updateResultingFilePath ())
        |> ignore

        this.ObservableForProperty(toLinq <@ fun vm -> vm.SelectedDestinationDirectory @>)
            .Subscribe(fun _ -> updateResultingFilePath ())
        |> ignore

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

    member __.DestinationDirectories = destinationDirectories

    member __.SelectedDestinationDirectory
        with get ()  : DirectoryInfo= selectedDestinationDirectory
        and set value = this.RaiseAndSetIfChanged(&selectedDestinationDirectory, value, nameof <@ __.SelectedDestinationDirectory @>) |> ignore

    member __.ResultingFilePath
        with get () = resultingFilePath
        and set value = this.RaiseAndSetIfChanged(&resultingFilePath, value, nameof <@ __.ResultingFilePath @>) |> ignore
