namespace FilenameEmbeddedMetadataOrganizer.UIHelper

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data

open FilenameEmbeddedMetadataOrganizer.ViewModels

type FeaturesTreeViewItemTemplateSelector() =
    inherit DataTemplateSelector()

    static member Instance = FeaturesTreeViewItemTemplateSelector()

    override __.SelectTemplate(item : obj, container : DependencyObject) =
        match item with
        | :? FeatureInstanceViewModel -> __.FeatureInstanceTemplate
        | :? FeatureViewModel -> __.FeatureRootTemplate
        | _ -> failwith "Grr"

    member val FeatureRootTemplate = Unchecked.defaultof<DataTemplate> with get, set

    member val FeatureInstanceTemplate = Unchecked.defaultof<DataTemplate> with get, set

type BooleanToVisibilityConverter() =
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
            let [| fullPath; baseDirectory |] = values |> Seq.cast<string> |> Seq.toArray

            let baseUri = Uri(if baseDirectory.EndsWith "\\" then baseDirectory else baseDirectory + "\\")
            let fullUri = Uri fullPath

            baseUri.MakeRelativeUri(fullUri).ToString().Replace("/", "\\") :> obj

        member this.ConvertBack(value: obj, targetTypes: System.Type [], parameter: obj, culture: System.Globalization.CultureInfo): obj [] =
            raise (System.NotImplementedException())
