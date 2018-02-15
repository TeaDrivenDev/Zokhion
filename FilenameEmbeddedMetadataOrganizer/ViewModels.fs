namespace FilenameEmbeddedMetadataOrganizer.ViewModels

open System
open System.Collections.ObjectModel
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open System.Windows

open ReactiveUI

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

    let reverseString (s : string) = s |> Seq.rev |> String.Concat
    let items = ObservableCollection<_>()

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
                    |> Seq.iter items.Add)
        |> ignore

    member __.BaseDirectory
        with get () = baseDirectory
        and set value = this.RaiseAndSetIfChanged(&baseDirectory, value, nameof <@ any<MainWindowViewModel>.BaseDirectory @>) |> ignore

    member __.Items = items