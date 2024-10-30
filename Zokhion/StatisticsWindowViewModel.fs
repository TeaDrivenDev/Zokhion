namespace TeaDriven.Zokhion.ViewModels

open ReactiveUI

open TeaDriven.Zokhion.Data

type StatisticsWindowViewModel(dataCache: DataCache) =
    inherit ReactiveObject()

    member __.Count = dataCache.AllNames.Count