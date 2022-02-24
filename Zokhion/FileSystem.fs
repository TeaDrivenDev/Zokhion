namespace TeaDriven.Zokhion


module FileSystem =
    open System
    open System.Collections.Concurrent
    open System.IO
    open System.Linq
    open System.Reactive.Linq
    open System.Reactive.Disposables

    open FSharp.Control.Reactive

    open Reactive.Bindings

    [<RequireQualifiedAccess>]
    module Directory =
        let inline exists path = Directory.Exists path

        let inline enumerateFiles directory =
            Directory.EnumerateFiles(directory, "*.*", SearchOption.AllDirectories)

        let inline getDirectories directory = Directory.GetDirectories directory

        let inline getFiles directory =
            Directory.GetFiles(directory, "*.*", SearchOption.AllDirectories)

    [<RequireQualifiedAccess>]
    module File =
        let inline delete path = File.Delete path

        let inline exists path = File.Exists path

        let inline move sourceFileName destinationFileName =
            File.Move(sourceFileName, destinationFileName)

        let inline readAllLines path = File.ReadAllLines(path)

        let inline writeAllLines path (lines: #seq<string>) =
            File.WriteAllLines(path, lines)

    [<RequireQualifiedAccess>]
    module Path =
        let inline combine parts = Path.Combine parts

        let inline getDirectoryName path = Path.GetDirectoryName path

        let inline getExtension path = Path.GetExtension path

        let inline getFileName path = Path.GetFileName path

        let inline getFileNameWithoutExtension path =
            Path.GetFileNameWithoutExtension path

    type FileChange = Added | Removed

    type FileSystemCacheStatus =
        | Empty
        | Initializing of progress: float
        | Ready

    [<AllowNullLiteral>]
    type FileInfoCopy(fileInfo: FileInfo) =
        let name = fileInfo.Name
        let fullName = fileInfo.FullName
        let directoryName = fileInfo.DirectoryName
        let creationTime = fileInfo.CreationTime
        let lastWriteTime = fileInfo.LastWriteTime
        let length = fileInfo.Length

        member __.Name = name
        member __.FullName = fullName
        member __.DirectoryName = directoryName
        member __.CreationTime = creationTime
        member __.LastWriteTime = lastWriteTime
        member __.Length = length

        new(filePath: string) = FileInfoCopy(FileInfo(filePath))

    [<AllowNullLiteral>]
    type DirectoryInfoCopy(directoryInfo: System.IO.DirectoryInfo) =
        let fullName = directoryInfo.FullName
        let name = directoryInfo.Name
        
        member __.FullName = fullName
        member __.Name = name

        // TODO Remove/Change
        member __.NumberOfFiles = directoryInfo.GetFiles().Length

        new (directoryPath: string) = DirectoryInfoCopy(DirectoryInfo(directoryPath))

    type FileSystemCache() =
        let compositeDisposable = new CompositeDisposable()

        let watcher =
            new FileSystemWatcher(EnableRaisingEvents = false, IncludeSubdirectories = true)
        let fileSystemChanges =
            Observable.Create
                (fun observer ->
                    [
                        watcher.Created |> Observable.map (fun e -> [ e.FullPath, Added ])
                        watcher.Deleted |> Observable.map (fun e -> [ e.FullPath, Removed ])
                        watcher.Renamed
                        |> Observable.map (fun e -> [ e.OldFullPath, Removed; e.FullPath, Added ])
                    ]
                    |> Observable.mergeSeq
                    |> Observable.subscribeObserver observer)

        let mutable directories = Unchecked.defaultof<ConcurrentDictionary<_, _>>

        let mutable status = Unchecked.defaultof<ReactiveProperty<_>>

        let mutable fileSystemUpdates = Unchecked.defaultof<IObservable<_>>

        do
            compositeDisposable.Add watcher

            status <- new ReactiveProperty<_>(Empty)

        interface IDisposable with
            member __.Dispose() =
                compositeDisposable.Dispose()

        member __.Status = status

        member __.DirectoriesWithFiles =
            directories
            |> Option.ofObj
            |> Option.map (fun directories -> directories |> Seq.toList)
            |> Option.defaultValue []

        member __.FileSystemUpdates = fileSystemUpdates

        member __.Initialize(baseDirectory: string) =
            status.Value <- Initializing 0.
            watcher.EnableRaisingEvents <- false

            async {
                let subDirectories = Directory.GetDirectories baseDirectory

                let data =
                    subDirectories
                    |> Array.mapi
                        (fun index directory ->
                            let files = Directory.GetFiles directory

                            status.Value <- Initializing (float index/float subDirectories.Length)

                            directory, files |> Array.map FileInfoCopy |> Array.toList)

                directories <- ConcurrentDictionary(data.ToDictionary(fst, snd))

                status.Value <- Ready

                let connectableObservable =
                    fileSystemChanges
                    |> Observable.iter
                        (fun changes ->
                            changes
                            |> List.groupBy (fst >> Path.getDirectoryName)
                            |> List.iter
                                (fun (directory, files) ->
                                    let filesInDirectory =
                                        match directories.TryGetValue directory with
                                        | true, filesInDirectory -> filesInDirectory
                                        | false, _ -> []
                                    
                                    let updatedFilesInDirectory =
                                        (filesInDirectory, files)
                                        ||> List.fold
                                            (fun acc (file, fileChange) ->
                                                match fileChange with
                                                | Added -> FileInfoCopy file :: acc
                                                | Removed -> 
                                                    acc 
                                                    |> List.filter 
                                                        (fun fileHere -> fileHere.FullName <> file))

                                    directories.[directory] <- updatedFilesInDirectory))
                    |> Observable.multicast Subject.broadcast

                connectableObservable.Connect() |> ignore
                fileSystemUpdates <- connectableObservable

                watcher.Path <- baseDirectory
                watcher.EnableRaisingEvents <- true
            }
