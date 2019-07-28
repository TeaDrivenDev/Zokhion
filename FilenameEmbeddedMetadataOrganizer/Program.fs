namespace FilenameEmbeddedMetadataOrganizer

open System
open System.ComponentModel
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Threading

open FsXaml

open FilenameEmbeddedMetadataOrganizer.ViewModels

type Win32Window(handle: IntPtr) =
    interface System.Windows.Forms.IWin32Window with
        member __.Handle: IntPtr = handle

type MainWindowBase = XAML<"MainWindow.xaml">

type MainWindow() as this =
    inherit MainWindowBase()

    let mutable dragStart = Unchecked.defaultof<Point>
    let draggedItems = ResizeArray<_>()

    let onClosing (_: CancelEventArgs) = this.ViewModel.Shutdown()

    do
        this.Closing.Add onClosing

    member __.ViewModel: MainWindowViewModel = __.DataContext :?> MainWindowViewModel

    override __.SelectFolder_Click (_: obj, e: RoutedEventArgs) =
        use folderBrowserDialog = new Forms.FolderBrowserDialog()
        folderBrowserDialog.ShowNewFolderButton <- false

        if not <| String.IsNullOrWhiteSpace __.ViewModel.BaseDirectory.Value
            && System.IO.Directory.Exists __.ViewModel.BaseDirectory.Value
        then
            folderBrowserDialog.SelectedPath <- __.ViewModel.BaseDirectory.Value

        let win = Win32Window(System.Windows.Interop.WindowInteropHelper(__).Handle)
        let result = folderBrowserDialog.ShowDialog win

        if result = Forms.DialogResult.OK
        then
            __.ViewModel.BaseDirectory.Value <- folderBrowserDialog.SelectedPath

    member __.Features_SelectedItemChanged (_: obj, e: RoutedPropertyChangedEventArgs<obj>) =
        match e.NewValue with
        | :? FeatureViewModel as item -> this.ViewModel.SelectedFeature.Value <- item
        | _ -> ()

    override __.SearchString_KeyUp(sender: obj, e: Input.KeyEventArgs) =
        if e.Key = Input.Key.Escape
        then
            (sender :?> TextBox).Text <- ""
            e.Handled <- true

    override __.OriginalFileName_SelectionChanged(sender: obj, e: RoutedEventArgs) =
        __.ViewModel.OriginalFileNameSelectedText.Value <-
            (sender :?> TextBox).SelectedText.Trim()

    override __.NewFileName_SelectionChanged(sender: obj, e: RoutedEventArgs) =
        __.ViewModel.NewFileNameSelectedText.Value <-
            (sender :?> TextBox).SelectedText.Trim()

    override __.NewNameToAdd_KeyUp(sender: obj, e: Input.KeyEventArgs) =
        let sender = sender :?> TextBox

        match e.Key with
        | Input.Key.Enter ->
            __.ViewModel.AddName sender.Text
            e.Handled <- true
        | Input.Key.Escape ->
            __.ViewModel.ClearNewNameToAdd()
            e.Handled <- true
        | _ -> ()

    // see https://stackoverflow.com/a/29028817/236507
    override __.InstanceName_KeyUp(sender: obj, e: Input.KeyEventArgs) =
        let sender = sender :?> TextBox

        match e.Key with
        | Key.Escape ->
            sender.Text <- ""
            e.Handled <- true
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

            e.Handled <- true
        | _ -> ()

    override __.InstanceCode_KeyUp(sender: obj, e: Input.KeyEventArgs) =
        __.InstanceName_KeyUp(sender, e)

    // Drag and drop implementation from http://codebrewery.blogspot.de/2010/06/drag-and-drop-files-from-wpf-listviews.html
    // see also https://stackoverflow.com/a/25123410/236507
    member __.FilesGrid_PreviewMouseLeftButtonDown(sender: obj, e: MouseButtonEventArgs) =
        let filesGrid = sender :?> DataGrid

        dragStart <- e.GetPosition null
        draggedItems.Clear()
        draggedItems.AddRange(filesGrid.SelectedItems |> Seq.cast<obj>)

    member __.FilesGrid_MouseMove(sender: obj, e: MouseEventArgs) =
        let position = e.GetPosition null
        let dragDifference = dragStart - position
        let filesGrid = sender :?> DataGrid

        if e.LeftButton = MouseButtonState.Pressed
            && Math.Abs dragDifference.X > SystemParameters.MinimumHorizontalDragDistance
            && Math.Abs dragDifference.Y > SystemParameters.MinimumVerticalDragDistance
            && filesGrid.SelectedItems.Count > 0
        then
            draggedItems
            |> Seq.filter (filesGrid.SelectedItems.Contains >> not)
            |> Seq.iter (filesGrid.SelectedItems.Add >> ignore)

            let files =
                draggedItems
                |> Seq.cast<System.IO.FileInfo>
                |> Seq.map (fun fileInfo -> fileInfo.FullName)
                |> Seq.toArray

            let dataObject = DataObject(DataFormats.FileDrop, files)

            try
                DragDrop.DoDragDrop(filesGrid, dataObject, DragDropEffects.Copy)
                |> ignore
            with _ -> ()

            e.Handled <- true

    override __.SearchResultDirectory_OnPreviewMouseLeftButtonUp(sender: obj, args: MouseButtonEventArgs) =
        match sender with
        | :? TextBlock as textBlock ->
            match textBlock.DataContext with
            | :? System.IO.FileInfo as fileInfo -> Some fileInfo.DirectoryName
            | _ -> None
        | _ -> None
        |> Option.iter __.ViewModel.CreateSearchTabForDirectory

type AppBase = XAML<"App.xaml">

type App() =
    inherit AppBase()

    member __.Application_Startup (_: obj, _: StartupEventArgs) =
        MainWindow().Show()

module Program =
    [<STAThread>]
    [<EntryPoint>]
    let main argv =
        App().Run()
