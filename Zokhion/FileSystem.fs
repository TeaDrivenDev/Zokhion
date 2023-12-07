namespace TeaDriven.Zokhion

module FileSystem =
    open System
    open System.Collections.Concurrent
    open System.IO
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

        let isSubDirectoryOf (baseDirectory: string) (subDirectory: string) =
            let normalizedPath =
                Path.GetFullPath(subDirectory.Replace('/', '\\')).TrimEnd('\\') + "\\"

            let normalizedBaseDirPath =
                Path.GetFullPath(baseDirectory.Replace('/', '\\')).TrimEnd('\\') + "\\"

            normalizedPath.StartsWith(normalizedBaseDirPath, StringComparison.OrdinalIgnoreCase)

    type FileChange = Added | Removed

    type FileSystemCacheStatus =
        | Empty
        | Initializing of progress: float
        | Ready

    [<AllowNullLiteral>]
    type FileInfoCopy(
        name: string,
        fullName: string,
        directoryName: string,
        creationTime: DateTime,
        lastWriteTime: DateTime,
        length: int64) =
        member __.Name = name
        member __.FullName = fullName
        member __.DirectoryName = directoryName
        member __.CreationTime = creationTime
        member __.LastWriteTime = lastWriteTime
        member __.Length = length

        static member FromFilePath(filePath: string) =
            let fileInfo = FileInfo filePath

            FileInfoCopy(
                fileInfo.Name,
                fileInfo.FullName,
                fileInfo.DirectoryName,
                fileInfo.CreationTime,
                fileInfo.LastWriteTime,
                fileInfo.Length)

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
                        |> Observable.map
                            (fun e ->
                                if not (Directory.Exists e.OldFullPath) && not (Directory.Exists e.FullPath)
                                then [ e.OldFullPath, Removed; e.FullPath, Added ]
                                else [])
                    ]
                    |> Observable.mergeSeq
                    |> Observable.filter (List.isEmpty >> not)
                    |> Observable.distinctUntilChanged
                    |> Observable.subscribeObserver observer)

        let mutable files = Unchecked.defaultof<ConcurrentDictionary<_, _>>

        let mutable status = Unchecked.defaultof<ReactiveProperty<_>>

        let mutable fileSystemUpdates = Unchecked.defaultof<IObservable<_>>

        do
            compositeDisposable.Add watcher

            status <- new ReactiveProperty<_>(Empty)

        interface IDisposable with
            member __.Dispose() =
                compositeDisposable.Dispose()

        member __.Status = status

        member __.Files =
            files
            |> Option.ofObj
            |> Option.map Seq.toList
            |> Option.defaultValue []

        member __.FileSystemUpdates = fileSystemUpdates

        member __.IsFileSystemWatcherAlive = watcher.EnableRaisingEvents

        member __.FileSystemWatcherPath = watcher.Path

        member __.Initialize(baseDirectory: string) =
            status.Value <- Initializing 0.
            watcher.EnableRaisingEvents <- false

            files |> Option.ofObj |> Option.iter (fun files -> files.Clear())

            async {
                let subDirectories = Directory.GetDirectories baseDirectory

                let data =
                    subDirectories
                    |> Array.mapi
                        (fun index files ->
                            let files = Directory.GetFiles files

                            status.Value <-
                                Initializing (float index / float subDirectories.Length * 100.)

                            files
                            |> Array.map (fun file -> file, FileInfoCopy.FromFilePath file))
                    |> Array.concat

                files <- data |> dict |> ConcurrentDictionary

                status.Value <- Ready

                let connectableObservable =
                    fileSystemChanges
                    |> Observable.iter
                        (fun changes ->
                            changes
                            |> List.filter (fst >> Directory.Exists >> not)
                            |> List.iter
                                (fun (file, fileChange) ->
                                    match fileChange with
                                    | Added -> files.[file] <- FileInfoCopy.FromFilePath file
                                    | Removed -> files.TryRemove file |> ignore))
                    |> Observable.multicast Subject.broadcast

                connectableObservable.Connect() |> ignore
                fileSystemUpdates <- connectableObservable

                watcher.Path <- baseDirectory
                watcher.EnableRaisingEvents <- true
            }

        member __.ReviveFileSystemWatcher() =
            if status.Value = Ready then watcher.EnableRaisingEvents <- true
