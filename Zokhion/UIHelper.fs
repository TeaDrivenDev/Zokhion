namespace TeaDriven.Zokhion.UIHelper

open System
open System.IO
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Windows.Data

open TeaDriven.Zokhion.ViewModels

module Utilities =
    let bytesToMegabytes (value: float) = value / (1024. * 1024.)

type FeaturesTreeViewItemTemplateSelector() =
    inherit DataTemplateSelector()

    override __.SelectTemplate(item: obj, container: DependencyObject) =
        match item with
        | :? FeatureInstanceViewModel -> __.FeatureInstanceTemplate
        | :? FeatureViewModel -> __.FeatureRootTemplate
        | _ -> failwith "Grr"

    member val FeatureRootTemplate = Unchecked.defaultof<DataTemplate> with get, set

    member val FeatureInstanceTemplate = Unchecked.defaultof<DataTemplate> with get, set

type InvertableBooleanToVisibilityConverter() =
    static member Instance = InvertableBooleanToVisibilityConverter() :> IValueConverter

    interface IValueConverter with
        member this.Convert(value: obj, targetType: System.Type, parameter: obj, culture: System.Globalization.CultureInfo): obj =
            if value = DependencyProperty.UnsetValue
            then Visibility.Visible
            else
                let show = System.Convert.ToBoolean value
                let invert = System.Convert.ToBoolean parameter

                if show <> invert
                then Visibility.Visible
                else Visibility.Collapsed
            :> obj

        member this.ConvertBack(value: obj, targetType: System.Type, parameter: obj, culture: System.Globalization.CultureInfo): obj =
            raise (System.NotSupportedException())

// see https://weblog.west-wind.com/posts/2010/Dec/20/Finding-a-Relative-Path-in-NET
type RelativePathConverter() =
    static member Instance = RelativePathConverter() :> IMultiValueConverter

    interface IMultiValueConverter with
        member this.Convert(values: obj [], targetType: System.Type, parameter: obj, culture: System.Globalization.CultureInfo): obj =
            match values with
            | [| :? string as fullPath; :? string as baseDirectory |] ->
                let baseUri = Uri(if baseDirectory.EndsWith "\\" then baseDirectory else baseDirectory + "\\")
                let fullUri = Uri fullPath

                baseUri.MakeRelativeUri(fullUri).ToString().Replace("/", "\\")
                |> System.Web.HttpUtility.UrlDecode
            | _ -> ""
            :> obj

        member this.ConvertBack(value: obj, targetTypes: System.Type [], parameter: obj, culture: System.Globalization.CultureInfo): obj [] =
            raise (System.NotSupportedException())

type BytesToMegabytesConverter() =
    static member Instance = BytesToMegabytesConverter() :> IValueConverter

    interface IValueConverter with
        member this.Convert(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            if value = DependencyProperty.UnsetValue
            then 0.
            else value |> System.Convert.ToDouble |> Utilities.bytesToMegabytes
            :> obj

        member this.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            raise (System.NotSupportedException())

// see https://stackoverflow.com/a/7290188/236507
type IndexOfConverter() =
    static member Instance = IndexOfConverter() :> IMultiValueConverter

    interface IMultiValueConverter with
        member this.Convert(values: obj [], targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            let itemsControl = values.[0] :?> ItemsControl
            let item = values.[1]
            let itemContainer = itemsControl.ItemContainerGenerator.ContainerFromItem item

            let oneBasedIndex = System.Convert.ToBoolean parameter

            if isNull itemContainer
            then Binding.DoNothing
            else
                if oneBasedIndex then 1 else 0
                + itemsControl.ItemContainerGenerator.IndexFromContainer itemContainer
                |> string
                :> obj

        member this.ConvertBack(value: obj, targetTypes: Type [], parameter: obj, culture: Globalization.CultureInfo): obj [] =
            targetTypes |> Array.map (fun _ -> Binding.DoNothing)

type AllItemsEqualConverter() =
    static member Instance = AllItemsEqualConverter() :> IMultiValueConverter

    interface IMultiValueConverter with
        member this.Convert(values: obj [], targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            values
            |> Array.filter ((<>) DependencyProperty.UnsetValue)
            |> Array.toList
            |> function
                | head :: tail -> tail |> List.forall ((=) head)
                | [] -> false
            :> obj

        member this.ConvertBack(value: obj, targetTypes: Type [], parameter: obj, culture: Globalization.CultureInfo): obj [] =
            raise (System.NotSupportedException())

type NameHasFeaturesConverter() =
    static member private FeatureRegex = Regex(@"\[\.(?<features>.+)\.\]$", RegexOptions.Compiled)

    static member Instance = NameHasFeaturesConverter() :> IValueConverter

    interface IValueConverter with
        member this.Convert(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            match value with
            | :? string as name ->
                name
                |> Path.GetFileNameWithoutExtension
                |> NameHasFeaturesConverter.FeatureRegex.IsMatch
            | _ -> false
            :> obj

        member this.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            raise (System.NotSupportedException())

type FeatureToDisplayStringConverter() =
    static member Instance = FeatureToDisplayStringConverter() :> IValueConverter

    interface IValueConverter with
        member this.Convert(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            let featureViewModel = value :?> FeatureViewModel

            featureViewModel.Include
            |> Option.map (fun toInclude -> " ➔ " + toInclude)
            |> Option.defaultValue ""
            |> sprintf "%s: %s%s" featureViewModel.FeatureName featureViewModel.FeatureCode
            :> obj

        member this.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            raise (System.NotSupportedException())

type BindingProxy() =
    inherit Freezable()

    static let DataProperty =
        DependencyProperty.Register("Data", typeof<obj>, typeof<BindingProxy>, UIPropertyMetadata(null));

    override __.CreateInstanceCore() = BindingProxy() :> Freezable

    member __.Data
        with get () = __.GetValue(DataProperty)
        and set value = __.SetValue(DataProperty, value)

type FileChange =
    | NoChange = 0
    | Renamed = 1
    | Moved = 2
    | Deleted = 3

type FileChangesToEnumConverter() =
    static member Instance = FileChangesToEnumConverter() :> IMultiValueConverter

    interface IMultiValueConverter with
        member this.Convert(values: obj [], targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            match values with
            | [| :? FileInfo as fileInfo; :? FileChanges as fileChanges |] ->
                match fileChanges.RenamedFiles.TryGetValue fileInfo.FullName with
                | true, renamedFile ->
                    if Path.GetDirectoryName renamedFile.OriginalFile.FullName = Path.GetDirectoryName renamedFile.NewFilePath
                    then FileChange.Renamed
                    else FileChange.Moved
                | false, _ ->
                    match fileChanges.DeletedFiles.TryGetValue fileInfo.FullName with
                    | true, _ -> FileChange.Deleted
                    | _ -> FileChange.NoChange
            | _ -> FileChange.NoChange
            :> obj

        member this.ConvertBack(value: obj, targetTypes: Type [], parameter: obj, culture: Globalization.CultureInfo): obj [] =
            raise (System.NotSupportedException())

type FilesToUniqueCountConverter() =
    static member Instance = FilesToUniqueCountConverter() :> IValueConverter

    interface IValueConverter with
        member this.Convert(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            match value with
            | :? (FileViewModel seq) as files ->
                files
                |> Seq.distinctBy (fun vm -> vm.FileInfo.FullName)
                |> Seq.length
            | _ -> 0
            :> obj

        member this.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            raise (System.NotSupportedException())

type FilesToTotalSizeConverter() =
    static member Instance = FilesToTotalSizeConverter() :> IValueConverter

    interface IValueConverter with
        member this.Convert(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            match value with
            | :? (FileViewModel seq) as files ->
                files
                |> Seq.distinctBy (fun vm -> vm.FileInfo.FullName)
                |> Seq.sumBy (fun fi -> fi.Length)
                |> float |> Utilities.bytesToMegabytes
            | _ -> 0.
            :> obj

        member this.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            raise (System.NotSupportedException())
