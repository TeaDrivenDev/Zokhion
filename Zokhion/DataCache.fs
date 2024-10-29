namespace TeaDriven.Zokhion

open System.Reactive.Disposables
open System.Text.RegularExpressions

open DynamicData

open TeaDriven.Zokhion.FileSystem
open TeaDriven.Zokhion.LoggingTypes
open TeaDriven.Zokhion.ViewModels

module Data =
    type DataCache(fileSystemCache: FileSystemCache, log: (LogLevel -> Activity -> string -> unit)) =
        let fileChangesSubscription = new SerialDisposable()

        let allNames = new SourceCache<NameViewModel, _>(fun vm -> vm.Name.Value)

        let namesPartRegex = Regex(@"\(\.(?<names>.+)\.\)", RegexOptions.Compiled)

        let parseNames fileName =
            let m = namesPartRegex.Match fileName

            if m.Success
            then m.Groups.["names"].Value.Split([| '.' |]) |> Array.toList
            else []

        let initializeNamesList (files: string seq) =
            allNames.Clear()

            let names =
                files
                |> Seq.collect (Path.getFileNameWithoutExtension >> parseNames)
                |> Seq.countBy id

            names
            |> Seq.iter
                (fun (name, count) ->
                    let nameViewModel =
                        NameViewModel(
                            name,
                            isSelected=false,
                            isNewlyDetected=false,
                            isAdded=false)

                    nameViewModel.Count.Value <- count

                    allNames.AddOrUpdate nameViewModel)

        let getNameViewModel name =
            let name = trim name

            allNames.Items
            |> Seq.tryFind (fun viewModel -> toUpper viewModel.Name.Value = toUpper name)
            |> Option.defaultWith
                (fun () ->
                    let viewModel =
                        NameViewModel(
                            name,
                            isSelected=false,
                            isNewlyDetected=false,
                            isAdded=true,
                            IsPinned=true)

                    allNames.AddOrUpdate viewModel
                    viewModel)

        let updateNamesListOnFileSystemChanges observable =
            let toChangeCount = function Added -> 1 | Removed -> -1

            observable
            |> Observable.filter (List.isEmpty >> not)
            |> Observable.subscribe
                (fun changes ->
                    let namesWithChanges =
                        changes
                        |> List.collect
                            (fun (fileName, change) ->
                                parseNames fileName |> List.map (asFst (toChangeCount change)))
                        |> List.groupBy fst
                        |> List.map (fun (key, changes) -> key, changes |> List.sumBy snd)
                        |> List.filter (snd >> (<>) 0)

                    namesWithChanges
                    |> List.iter
                        (fun (name, changeCount) ->
                            let viewModel = getNameViewModel name

                            let newCount = viewModel.Count.Value + changeCount

                            if newCount > 0
                            then viewModel.Count.Value <- newCount
                            else allNames.Remove viewModel))

        let initialize (directory: string) =
            async {
                allNames.Clear()

                do! fileSystemCache.Initialize directory

                fileSystemCache.Files
                |> Seq.map (fun (KeyValue(file, fileInfo)) -> file)
                |> initializeNamesList

                fileChangesSubscription.Disposable <-
                    updateNamesListOnFileSystemChanges fileSystemCache.FileSystemUpdates
            }

        let updateNamesList detectedNames =
            (detectedNames, allNames.Items |> Seq.toList)
            ||> fullOuterJoin toUpper (_.Name.Value >> toUpper)
            |> Seq.iter
                (function
                    | LeftOnly viewModel -> viewModel.IsSelected <- false
                    | RightOnly name ->
                        NameViewModel(
                            name,
                            isSelected=true,
                            isNewlyDetected=true,
                            isAdded=false)
                        |> allNames.AddOrUpdate
                    | JoinMatch (viewModel, name) ->
                        viewModel.Name.Value <- name
                        viewModel.IsSelected <- true)

        let getAllNames () =
            allNames.Items
            |> Seq.filter (_.IsNewlyDetected.Value >> not)
            |> Seq.map _.Name.Value
            |> Seq.toList

        let clearNewlyDetected () =
            allNames.Items
            |> Seq.filter _.IsNewlyDetected.Value
            |> Seq.toList
            |> List.iter allNames.Remove

        member __.AllNames = allNames

        member __.ClearNewlyDetected() = clearNewlyDetected()

        member __.GetNameViewModel(name: string) = getNameViewModel name

        member __.GetAllNames() = getAllNames ()

        member __.Initialize(directory: string) = initialize directory

        member __.UpdateNamesList(names: string seq) = updateNamesList names
