namespace FilenameEmbeddedMetadataOrganizer

open System
open System.Windows

open FsXaml

open FilenameEmbeddedMetadataOrganizer.ViewModels
open System.ComponentModel

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() as this =
    inherit MainWindowBase()

    let onClosing (_ : CancelEventArgs) = this.ViewModel.ShutDown()

    do
        this.Closing.Add onClosing

    member this.ViewModel : MainWindowViewModel = this.DataContext :?> MainWindowViewModel

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
