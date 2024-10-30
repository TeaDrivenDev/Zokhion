namespace TeaDriven.Zokhion.ViewModels

open System
open System.Collections.ObjectModel
open System.Reactive.Linq

open DynamicData

open ReactiveUI

open TeaDriven.Zokhion.Data

type StatisticsWindowViewModel(dataCache: DataCache) =
    inherit ReactiveObject()

    let mutable names = Unchecked.defaultof<ReadOnlyObservableCollection<_>>

    do
        dataCache.AllNames.Connect()
            .AutoRefresh()
            .ObserveOn(RxApp.MainThreadScheduler)
            .Bind(&names)
            .Subscribe()
        |> ignore

    member __.Count = dataCache.AllNames.Count
    member __.Names = names
