namespace TeaDriven.Zokhion.ViewModels

open System
open System.Collections.Generic

open DynamicData.Binding

open ReactiveUI

open Reactive.Bindings

open TeaDriven.Zokhion

[<AllowNullLiteral>]
type FeatureViewModel(feature: Feature) as this =
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

    member __.ResetExpanded () = ()
        // __.IsExpanded.Value <- __.Instances |> Seq.exists (fun vm -> vm.IsSelected)

and [<AllowNullLiteral>]
    FeatureInstanceViewModel(feature: Feature, instance: FeatureInstance) =
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

//and NewFeatureInstanceViewModel(instanceName: string,
//                                instanceCode: string,
//                                incomingUpdates: IObservable<FeatureInstanceIncomingUpdate>) as this =

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

//    new (incomingUpdates: IObservable<FeatureInstanceIncomingUpdate>) =
//        NewFeatureInstanceViewModel("", "", incomingUpdates)

type NewFeatureInstanceViewModel(instanceName: string, instanceCode: string) =
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

    new (feature: FeatureInstance) =
        NewFeatureInstanceViewModel(feature.Name, feature.Code)

[<AllowNullLiteral>]
type NameViewModel(name: string, isSelected: bool, isNewlyDetected: bool, isAdded: bool) =
    inherit AbstractNotifyPropertyChanged()

    let mutable xIsSelected = isSelected

    let mutable isPinned = false

    member val Name = new ReactiveProperty<_>(name)

    member __.IsSelected
        with get () = xIsSelected
        and set value = __.SetAndRaise(&xIsSelected, value, nameof <@ __.IsSelected @>)

    member __.IsPinned
        with get () = isPinned
        and set value = __.SetAndRaise(&isPinned, value, nameof <@ __.IsPinned @>)

    member val IsNewlyDetected = new ReactiveProperty<_>(isNewlyDetected)

    member val IsAdded = new ReactiveProperty<_>(isAdded)

    member val Count = new ReactiveProperty<_>(0)

    member __.ClearNewFlagCommand =
        ReactiveCommand.Create(fun () ->
            __.IsNewlyDetected.Value <- false
            __.IsAdded.Value <- true)

    member __.PinCommand =
        ReactiveCommand.Create(fun () -> __.IsPinned <- not __.IsPinned)

type NameViewModelComparer() =
    interface IComparer<NameViewModel> with
        member __.Compare (a, b) =
            let byFlag flagA flagB =
                match flagA, flagB with
                | true, false -> Some -1
                | false, true -> Some 1
                | _ -> None

            byFlag a.IsSelected b.IsSelected
            |> Option.defaultWith (fun () ->
                byFlag a.IsNewlyDetected.Value b.IsNewlyDetected.Value
                |> Option.defaultWith (fun () ->
                    byFlag a.IsPinned b.IsPinned
                    |> Option.defaultWith (fun () -> a.Name.Value.CompareTo b.Name.Value)))
