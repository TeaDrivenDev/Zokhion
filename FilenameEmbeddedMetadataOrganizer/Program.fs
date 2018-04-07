namespace FilenameEmbeddedMetadataOrganizer

open System
open System.ComponentModel
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Threading

open FsXaml

open FilenameEmbeddedMetadataOrganizer.ViewModels

type Win32Window(handle : IntPtr) =
    interface System.Windows.Forms.IWin32Window with
        member __.Handle : IntPtr = handle

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() as this =
    inherit MainWindowBase()

    let onClosing (_ : CancelEventArgs) = this.ViewModel.Shutdown()

    do
        this.Closing.Add onClosing

    member this.ViewModel : MainWindowViewModel = this.DataContext :?> MainWindowViewModel

    override this.SelectFolder_Click (_ : obj, e : RoutedEventArgs) =
        use folderBrowserDialog = new Forms.FolderBrowserDialog()
        folderBrowserDialog.ShowNewFolderButton <- false

        if not <| String.IsNullOrWhiteSpace this.ViewModel.BaseDirectory.Value
            && System.IO.Directory.Exists this.ViewModel.BaseDirectory.Value
        then
            folderBrowserDialog.SelectedPath <- this.ViewModel.BaseDirectory.Value

        let win = Win32Window(System.Windows.Interop.WindowInteropHelper(this).Handle)
        let result = folderBrowserDialog.ShowDialog win

        if result = Forms.DialogResult.OK
        then
            this.ViewModel.BaseDirectory.Value <- folderBrowserDialog.SelectedPath

    member this.Features_SelectedItemChanged (_ : obj, e : RoutedPropertyChangedEventArgs<obj>) =
        match e.NewValue with
        | :? FeatureViewModel as item -> this.ViewModel.SelectedFeature.Value <- item
        | _ -> ()

    override __.SearchText_KeyUp(sender : obj, e : Input.KeyEventArgs) =
        if e.Key = Input.Key.Escape
        then (sender :?> TextBox).Text <- ""

    // see https://stackoverflow.com/a/29028817/236507
    override __.InstanceName_KeyUp(sender : obj, e : Input.KeyEventArgs) =
        let sender = sender :?> TextBox

        match e.Key with
        | Key.Escape -> sender.Text <- ""
        | Key.Enter ->
            let focusDirection =
                match sender.Name with
                | "InstanceName" -> FocusNavigationDirection.Down
                | _ -> FocusNavigationDirection.Next

            let request = TraversalRequest focusDirection

            match Keyboard.FocusedElement with
            | :? UIElement as focused ->
                let predicted = focused.PredictFocus FocusNavigationDirection.Down

                match predicted with
                | :? TextBox as textBox
                    when textBox.Name = "InstanceName" || textBox.Name = "InstanceCode" -> ()

                | _ -> __.ViewModel.AddFeatureInstanceRow()

                focused.Dispatcher.BeginInvoke(DispatcherPriority.Input, Threading.ThreadStart(fun () ->
                    focused.MoveFocus request |> ignore))
                |> ignore
            | _ -> ()
        | _ -> ()

    override __.InstanceCode_KeyUp(sender : obj, e : Input.KeyEventArgs) =
        __.InstanceName_KeyUp(sender, e)

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
