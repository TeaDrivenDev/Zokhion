namespace FilenameEmbeddedMetadataOrganizer.UIHelper

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data

open FilenameEmbeddedMetadataOrganizer.ViewModels

type FeaturesTreeViewItemTemplateSelector() =
    inherit DataTemplateSelector()

    override __.SelectTemplate(item : obj, container : DependencyObject) =
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
                match System.Convert.ToBoolean value, System.Convert.ToBoolean parameter with
                | true, false | false, true -> Visibility.Visible
                | true, true | false, false -> Visibility.Collapsed
            :> obj

        member this.ConvertBack(value: obj, targetType: System.Type, parameter: obj, culture: System.Globalization.CultureInfo): obj =
            raise (System.NotImplementedException())

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
            raise (System.NotImplementedException())

type BytesToMegabytesConverter() =
    static member Instance = BytesToMegabytesConverter() :> IValueConverter

    interface IValueConverter with
        member this.Convert(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            if value = DependencyProperty.UnsetValue
            then 0.
            else (System.Convert.ToDouble value) / 1_000_000.
            :> obj

        member this.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: Globalization.CultureInfo): obj =
            raise (System.NotImplementedException())

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
                |> string :> obj

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
                | head :: tail ->
                    tail |> List.exists ((<>) head) |> not
                | [] -> false
            :> obj

        member this.ConvertBack(value: obj, targetTypes: Type [], parameter: obj, culture: Globalization.CultureInfo): obj [] =
            raise (System.NotImplementedException())
