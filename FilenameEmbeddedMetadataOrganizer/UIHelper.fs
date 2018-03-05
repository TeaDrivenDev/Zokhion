namespace FilenameEmbeddedMetadataOrganizer

open System.Windows
open System.Windows.Controls

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
