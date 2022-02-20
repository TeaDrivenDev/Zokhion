namespace TeaDriven.Zokhion

module FileSystem =
    open System
    open System.IO
    open System.Reactive.Disposables

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
