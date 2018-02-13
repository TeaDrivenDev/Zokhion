namespace FilenameEmbeddedMetadataOrganizer

open System

open FsXaml

type App = XAML<"App.xaml">
type MainWindow = XAML<"MainWindow.xaml">

module Program =
    [<STAThread>]
    [<EntryPoint>]
    let main argv =
        App().Run()