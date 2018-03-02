namespace FilenameEmbeddedMetadataOrganizer.ViewModels

open System
open System.Collections.ObjectModel
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open System.Windows

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

    let reverseString (s : string) = s |> Seq.rev |> String.Concat
    let directories = ObservableCollection<_>()
    let files = ObservableCollection<_>()

    do
        RxApp.MainThreadScheduler <- DispatcherScheduler(Application.Current.Dispatcher)

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

    member __.BaseDirectory
        with get () = baseDirectory
        and set value = this.RaiseAndSetIfChanged(&baseDirectory, value, nameof <@ any<MainWindowViewModel>.BaseDirectory @>) |> ignore

    member __.Directories = directories
    member __.Files = files

    member __.SelectedDirectory
        with get () = selectedDirectory
        and set value = this.RaiseAndSetIfChanged(&selectedDirectory, value, nameof <@ any<MainWindowViewModel>.SelectedDirectory @>) |> ignore

    member __.SelectedFile
        with get () = selectedFile
        and set value = this.RaiseAndSetIfChanged(&selectedFile, value, nameof <@ any<MainWindowViewModel>.SelectedFile @>) |> ignore