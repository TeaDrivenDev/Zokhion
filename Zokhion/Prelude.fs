namespace TeaDriven.Zokhion

[<AutoOpen>]
module Prelude =
    open System
    open System.Linq
    open System.Reactive.Disposables
    open System.Reactive.Subjects

    open FSharp.Control.Reactive

    type FullJoinResult<'TLeft, 'TRight> =
        | LeftOnly of 'TLeft
        | RightOnly of 'TRight
        | JoinMatch of 'TLeft * 'TRight

    [<AllowNullLiteral>]
    type JoinWrapper<'a>(value: 'a) =
        member __.Value = value

    let (|JoinWrapped|) (wrapper: JoinWrapper<_>) = wrapper.Value

    let asFst second first = first, second
    let asSnd first second = first, second

    let swap (a, b) = b, a

    let uncurry fn (a, b) = fn a b

    let join innerKeySelector outerKeySelector (inner: seq<'TInner>) (outer: seq<'TOuter>) =
        outer.Join(
            inner,
            Func<_, _> outerKeySelector,
            Func<_, _> innerKeySelector,
            fun outerItem innerItem -> outerItem, innerItem)

    let leftJoin innerKeySelector outerKeySelector (inner: seq<'TInner>) (outer: seq<'TOuter>) =
        query {
            for o in outer do
            leftOuterJoin i in inner on
                (outerKeySelector o = innerKeySelector i) into result
            for joined in result.DefaultIfEmpty() do
            select (o, joined |> Option.ofObj)
        }

    let fullOuterJoin innerKeySelector outerKeySelector (right: 'TRight seq) (left: 'TLeft seq) =
        let optionizeFirst (a, b) = Some a, b

        let valueInOuter =
            leftJoin innerKeySelector outerKeySelector right left
            |> Seq.map optionizeFirst

        let valueInInnerOnly =
            leftJoin outerKeySelector innerKeySelector left right
            |> Seq.filter (snd >> Option.isNone)
            |> Seq.map (optionizeFirst >> swap)

        Seq.append valueInOuter valueInInnerOnly
        |> Seq.map (function
            | Some leftItem, Some rightItem -> JoinMatch (leftItem, rightItem)
            | Some leftItem, None -> LeftOnly leftItem
            | None, Some rightItem -> RightOnly rightItem
            | None, None -> failwith "This can never happen.")

    let (|OfNull|) value = if isNull value then None else Some value

    let (|IsSome|) values = List.choose id values

    let nonEmptyString value =
        if String.IsNullOrWhiteSpace value then None else Some value

    let inline ( <&&> ) f g x = f x && g x

    /// A BehaviorSubject that only keeps a value fulfilling a certain condition
    type SelectiveBehaviorSubject<'T>(selector: 'T -> bool) =
        let compositeDisposable = new CompositeDisposable()

        let innerSubject = new System.Reactive.Subjects.Subject<'T>()

        let mutable lastValue = None

        do
            innerSubject
            |> Observable.subscribe (fun value ->
                if selector value then lastValue <- Some value)
            |> compositeDisposable.Add

            compositeDisposable.Add innerSubject

        interface ISubject<'T> with
            member this.OnCompleted(): unit =
                innerSubject.OnCompleted()
            member this.OnError(error: exn): unit =
                innerSubject.OnError error
            member this.OnNext(value: 'T): unit =
                innerSubject.OnNext value
            member this.Subscribe(observer: IObserver<'T>): IDisposable =
                lastValue
                |> Option.iter (fun command -> observer.OnNext command)

                let sub = innerSubject |> Observable.subscribeObserver observer
                compositeDisposable.Add sub
                sub

        interface IDisposable with
            member this.Dispose(): unit =
                compositeDisposable.Dispose()
