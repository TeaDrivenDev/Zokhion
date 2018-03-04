namespace FilenameEmbeddedMetadataOrganizer

open System
open System.Windows

open FsXaml

open FilenameEmbeddedMetadataOrganizer.ViewModels

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() =
    inherit MainWindowBase()

    override this.Window_OnClosing (_, _) =
        (this.DataContext :?> MainWindowViewModel).ShutDown()

type AppBase = XAML<"App.xaml">

type App() =
    inherit AppBase()

    member __.Application_Startup (_ : obj, _ : StartupEventArgs) =
        MainWindow().Show()

module Program =
    [<STAThread>]
    [<EntryPoint>]
    let main argv =
        App().Run()
