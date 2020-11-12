namespace TeaDriven.Zokhion.ViewModels

open System
open System.Reactive.Linq

open Reactive.Bindings

[<AutoOpen>]
module Utility =
    let withLatestFrom (observable2: IObservable<'T2>) (observable1: IObservable<'T1>) =
        observable1.WithLatestFrom(observable2, fun v1 v2 -> v1, v2)

    let xwhen (observable2: IObservable<_>) (observable1: IObservable<_>) =
        observable1
        |> withLatestFrom observable2
        |> Observable.filter snd
        |> Observable.map fst

    let containsAll parts (s: string) = parts |> List.forall s.Contains

    let split (separators: string []) (s: string) =
        s.Split(separators, StringSplitOptions.RemoveEmptyEntries)

    let toReadOnlyReactiveProperty (observable: IObservable<_>) =
        observable.ToReadOnlyReactiveProperty()

    let toReadOnlyReactivePropertyWithMode (mode: ReactivePropertyMode) (observable: IObservable<_>) =
        observable.ToReadOnlyReactiveProperty(mode=mode)

    let maxString maxLength (s: string) = s.Substring(0, Math.Min(s.Length, maxLength))
