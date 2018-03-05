namespace FilenameEmbeddedMetadataOrganizer

open System
open System.Windows

open FsXaml

open FilenameEmbeddedMetadataOrganizer.ViewModels

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() =
    inherit MainWindowBase()

    member this.ViewModel = this.DataContext :?> MainWindowViewModel

    override this.Window_OnClosing (_, _) =
        this.ViewModel.ShutDown()

    member this.Features_SelectedItemChanged (_ : obj, e : RoutedPropertyChangedEventArgs<obj>) =
        match e.NewValue with
        | :? FeatureViewModel as item -> this.ViewModel.SelectedFeature <- item
        | _ -> ()

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
