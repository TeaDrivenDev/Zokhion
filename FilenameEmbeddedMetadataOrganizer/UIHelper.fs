namespace FilenameEmbeddedMetadataOrganizer.UIHelper

open System.Windows
open System.Windows.Controls

open FilenameEmbeddedMetadataOrganizer.ViewModels
open System.Windows.Data

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
