namespace FilenameEmbeddedMetadataOrganizer

[<AutoOpen>]
module Prelude =
    open System
    open System.Linq

    type FullJoinResult<'TLeft, 'TRight> =
        | LeftOnly of 'TLeft
        | RightOnly of 'TRight
        | JoinMatch of 'TLeft * 'TRight

    [<AllowNullLiteral>]
    type JoinWrapper<'a>(value : 'a) =
        member __.Value = value

    let (|JoinWrapped|) (wrapper : JoinWrapper<_>) = wrapper.Value

    let asFst second first = first, second
    let asSnd first second = first, second

    let swap (a, b) = b, a

    let uncurry fn (a, b) = fn a b

    let join innerKeySelector outerKeySelector (inner : seq<'TInner>) (outer : seq<'TOuter>) =
        outer.Join(inner, Func<_, _> outerKeySelector, Func<_, _> innerKeySelector, fun outer inner -> outer, inner)

    let leftJoin innerKeySelector outerKeySelector (inner : seq<'TInner>) (outer : seq<'TOuter>) =
        query {
            for o in outer do
            leftOuterJoin i in inner on
                (outerKeySelector o = innerKeySelector i) into result
            for joined in result.DefaultIfEmpty() do
            select (o, joined |> Option.ofObj)
        }

    let fullOuterJoin innerKeySelector outerKeySelector (right : 'TRight seq) (left : 'TLeft seq) =
        let optionizeFirst (a, b) = Some a, b

        let valueInOuter =
            leftJoin innerKeySelector outerKeySelector right left
            |> Seq.map optionizeFirst

        let valueInInnerOnly =
            leftJoin outerKeySelector innerKeySelector left right
            |> Seq.filter (snd >> Option.isNone)
            |> Seq.map (optionizeFirst >> swap)

        Seq.append valueInOuter valueInInnerOnly
        |> Seq.map (function
            | Some leftItem, Some rightItem -> JoinMatch (leftItem, rightItem)
            | Some leftItem, None -> LeftOnly leftItem
            | None, Some rightItem -> RightOnly rightItem
            | None, None -> failwith "This can never happen.")

    let (|OfNull|) value = if isNull value then None else Some value
