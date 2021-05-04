module View.Post exposing (view, viewBurnOrTip, viewChainCard, viewComposeInput, viewReplyInput)

import Chain
import Element exposing (Color, Element, alignBottom, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Maybe.Extra exposing (unwrap)
import Misc
import Set
import Theme exposing (almostWhite, black, orange, white)
import Time exposing (Posix)
import TokenValue exposing (TokenValue)
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, slightRound, typeFont, whiteGlowAttributeSmall)
import View.Common exposing (chain, phaceElement, when, whenAttr, whenJust)
import View.Img
import View.Markdown


view :
    DisplayProfile
    -> Maybe Posix
    -> Posix
    -> Set.Set Types.PostKey
    -> Maybe Accounting
    -> Maybe BurnOrTipUX
    -> Maybe TooltipId
    -> Maybe String
    -> Maybe UserInfo
    -> Core
    -> Element Msg
view dProfile timestamp now replies accounting state tooltipState topic wallet post =
    let
        isMobile =
            dProfile == Mobile

        pad =
            if isMobile then
                10

            else
                30
    in
    [ viewHeader isMobile tooltipState accounting topic timestamp now post
    , viewCardMobile timestamp now post
        |> when isMobile
    , viewBody dProfile post
    , viewBottom replies post wallet state
    ]
        |> column
            [ width fill
            , spacing 10
            , Background.color black
            , Font.color white
            , whiteGlowAttributeSmall
            , padding pad
            , typeFont
            ]


viewBottom : Set.Set Types.PostKey -> Core -> Maybe UserInfo -> Maybe BurnOrTipUX -> Element Msg
viewBottom replies post wallet state =
    [ [ View.Img.speechBubble 17 almostWhite
      , Set.size replies
            |> Misc.formatReplies
            |> text
      ]
        |> row [ spacing 10, Font.size 23 ]
        |> linkToPost post.id
        |> el
            [ Element.alignRight
            , Element.alignBottom
            ]
        |> el
            [ Font.color almostWhite
            , Font.size 17
            , width fill
            , height fill
            ]
    , viewBurnOrTip post wallet state
    ]
        |> row [ width fill, spacing 10 ]


viewHeader : Bool -> Maybe TooltipId -> Maybe Accounting -> Maybe String -> Maybe Time.Posix -> Time.Posix -> Core -> Element Msg
viewHeader isMobile tooltipState accounting topic timestamp now post =
    [ phaceElement
        60
        post.author
        False
        (GotoView <| ViewUser post.author)
        |> el [ Element.alignTop ]
    , [ post.content.title
            |> whenJust
                (text
                    >> List.singleton
                    >> paragraph [ Font.size 30 ]
                    >> linkToPost post.id
                )
      , [ accounting
            |> whenJust (viewAccounting tooltipState post)
        , [ topic
                |> whenJust
                    (\t ->
                        Input.button [ Font.size 20, hover, width fill ]
                            { onPress = Just <| GotoView <| ViewTopic t
                            , label = View.Common.topic t
                            }
                    )
          , View.Common.timingOrSpinner now timestamp
          ]
            |> row [ spacing 10 ]
            |> when (not isMobile)
        ]
            |> row [ width fill, spaceEvenly ]
      ]
        |> column [ width fill, spacing 20 ]
    , viewChainCard () post
        |> el [ Element.alignTop ]
        |> when (not isMobile)
    ]
        |> row [ width fill, spacing 20 ]


viewAccounting : Maybe TooltipId -> Core -> Accounting -> Element Msg
viewAccounting tooltipState post data =
    [ viewAmount Theme.darkRed
        data.totalBurned
        { id = post.id, labelType = Burn }
    , viewAmount Theme.darkGreen
        data.totalTipped
        { id = post.id, labelType = Tip }
    ]
        |> row
            [ spacing 5
            , tooltipState
                |> whenJust
                    (\val ->
                        let
                            txt =
                                case val.labelType of
                                    Tip ->
                                        "Author earned $" ++ Misc.formatDollar data.totalTipped ++ " for this post"

                                    Burn ->
                                        "Author burned $" ++ Misc.formatDollar post.authorBurn ++ " + crowd amplified $" ++ Misc.formatDollar (TokenValue.sub data.totalBurned post.authorBurn)
                        in
                        [ text txt ]
                            |> paragraph
                                [ padding 10
                                , (if val.labelType == Burn then
                                    Theme.darkRed

                                   else
                                    Theme.darkGreen
                                  )
                                    |> Background.color
                                ]
                            |> when (val.id == post.id)
                    )
                |> Element.below
            ]


viewBurnOrTip : Core -> Maybe UserInfo -> Maybe BurnOrTipUX -> Element Msg
viewBurnOrTip post chain =
    let
        showActions =
            chain
                |> unwrap False (.chain >> (==) post.chain)
    in
    unwrap
        (viewButtons post
            |> when showActions
        )
        (\data ->
            viewBurnOrTipInput post data
                |> when (data.id == post.id)
        )


viewChainCard : a -> Core -> Element Msg
viewChainCard _ post =
    let
        block =
            "@"
                ++ String.fromInt post.id.block
                |> text

        col =
            Chain.getColor post.chain
    in
    Element.newTabLink
        [ hover
        , Background.color <| Theme.withAlpha 0.2 col
        , Border.width 1
        , Border.color <| Theme.withAlpha 0.5 <| col
        , Font.color <| Theme.withAlpha 0.5 <| Theme.white
        , Element.paddingXY 10 10
        , View.Attrs.sansSerifFont
        ]
        { url = Chain.txUrl post.chain post.txHash
        , label =
            [ [ chain post.chain
                    |> el [ Font.bold, Font.color col ]
              , [ block
                    |> el [ Font.size 14, alignBottom ]
                , View.Img.link 17 (Element.rgb 0.8 0.8 0.8)
                    |> el [ Element.alignRight ]
                ]
                    |> row [ spacing 5, width fill ]
              ]
                |> column [ spacing 5, Font.size 20 ]
            ]
                |> row
                    [ spacing 10
                    , Font.size 17
                    , width fill
                    ]
        }


viewCardMobile : Maybe Time.Posix -> Time.Posix -> Core -> Element Msg
viewCardMobile timestamp now post =
    let
        block =
            "@"
                ++ String.fromInt post.id.block
                |> text

        timing =
            View.Common.timingOrSpinner now timestamp

        col =
            Chain.getColor post.chain
    in
    Element.newTabLink
        [ hover
        , Background.color col
        , Font.color white
        , roundBorder
        , padding 10
        , View.Attrs.sansSerifFont
        , width fill
        ]
        { url = Chain.txUrl post.chain post.txHash
        , label =
            [ View.Common.chain post.chain
            , View.Common.verticalRule white
            , block
            , View.Common.verticalRule white
            , timing
            ]
                |> row
                    [ spaceEvenly
                    , Font.size 17
                    , width fill
                    ]
        }


linkToPost : PostId -> Element Msg -> Element Msg
linkToPost id elem =
    Input.button [ width fill, hover ]
        { onPress = Just <| GotoView <| ViewPost id
        , label = elem
        }


viewBody : DisplayProfile -> Core -> Element Msg
viewBody device post =
    post.content.body
        |> View.Markdown.renderString device
        |> el
            [ height <| px 100
            , View.Attrs.sansSerifFont
            , Element.clip
            , width fill
            , el
                [ View.Attrs.cappedHeight 50
                , width fill
                , Element.alignBottom
                , Background.gradient
                    { angle = degrees 0
                    , steps =
                        [ 0.9
                        , 0.8
                        , 0.7
                        , 0.6
                        , 0.5
                        , 0.4
                        , 0.3
                        ]
                            |> List.map Theme.blackAlpha
                    }
                ]
                Element.none
                |> Element.inFront
            ]
        |> linkToPost post.id


viewAmount : Color -> TokenValue -> TooltipId -> Element Msg
viewAmount color amount state =
    Input.button
        [ padding 5
        , Border.rounded 3
        , Font.size 22
        , Font.color white
        , Background.color color
        , hover
        , View.Attrs.help
        ]
        { onPress = Just <| ToggleTooltip state
        , label =
            [ View.Img.dollar 22 white
            , Misc.formatDollar amount
                |> text
                |> el [ Element.moveUp 1 ]
            ]
                |> row []
        }


viewBurnOrTipInput : Core -> BurnOrTipUX -> Element Msg
viewBurnOrTipInput post state =
    let
        name =
            Chain.getName post.chain

        title =
            case state.burnOrTip of
                Tip ->
                    "Tip " ++ name ++ " for this post, rewarding the author."

                Burn ->
                    "Burn " ++ name ++ " to increase the visibility of this post."

        isEmpty =
            String.isEmpty state.input
                || (state.input
                        |> String.toFloat
                        |> unwrap False ((==) 0.0)
                   )
    in
    [ [ text title ]
        |> paragraph []
    , [ View.Img.dollar 30 white
      , Input.text [ Font.color black ]
            { onChange = BurnOrTipUXInputChange
            , label = Input.labelHidden ""
            , placeholder =
                "00.00"
                    |> text
                    |> Input.placeholder []
                    |> Just
            , text = state.input
            }
      ]
        |> row [ spacing 5, width fill ]
    , state.error
        |> whenJust
            (text
                >> List.singleton
                >> paragraph
                    [ Background.color white
                    , Element.alignRight
                    , slightRound
                    , padding 10
                    , Font.color black
                    ]
            )
    , [ View.Common.cancel CancelPostInput
      , Input.button
            [ Background.color Theme.orange
            , padding 10
            , roundBorder
            , hover
            , Font.color black
            ]
            { onPress =
                if state.inProgress || isEmpty then
                    Nothing

                else
                    Just SubmitTipOrBurn
            , label =
                if state.inProgress then
                    View.Common.spinner 20 black
                        |> el [ centerX ]

                else
                    text "Submit"
            }
      ]
        |> row [ Element.alignRight, spacing 20 ]
    ]
        |> column
            [ Background.color black
            , spacing 10
            , padding 10
            , width fill
            , Font.color white
            , View.Attrs.sansSerifFont
            ]


viewButtons : Core -> Element Msg
viewButtons post =
    [ supportBurnButton post.id
    , supportTipButton post.id
    ]
        |> row [ spacing 10, Element.alignRight ]


supportTipButton : PostId -> Element Msg
supportTipButton postId =
    Input.button
        [ height <| px 40
        , Background.color Theme.darkGreen
        , width <| px 40
        , View.Attrs.title "Tip for this post, rewarding the author."
        , hover
        ]
        { onPress = Just <| StartBurnOrTipUX postId Types.Tip
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }


supportBurnButton : PostId -> Element Msg
supportBurnButton postId =
    Input.button
        [ height <| px 40
        , Background.color Theme.darkRed
        , width <| px 40
        , View.Attrs.title "Burn to increase the visibility of this post."
        , hover
        ]
        { onPress = Just <| StartBurnOrTipUX postId Types.Burn
        , label =
            View.Img.dollar 30 white
                |> el [ centerX, centerY ]
        }


viewComposeInput : String -> Bool -> DisplayProfile -> ComposeModel -> UserInfo -> Element Msg
viewComposeInput topicInput =
    viewComposePanel True
        ([ "Topic"
            |> text
            |> el [ centerY ]
            |> el
                [ Background.color orange
                , Border.roundEach
                    { bottomLeft = 5
                    , topLeft = 5
                    , bottomRight = 0
                    , topRight = 0
                    }
                , height fill
                , Element.paddingXY 10 0
                , sansSerifFont
                ]
         , Input.text
            [ Background.color white
            , width <| px 250
            , Border.roundEach
                { bottomLeft = 0
                , topLeft = 0
                , bottomRight = 5
                , topRight = 5
                }
            , padding 5
            , height <| px 35
            , Element.Events.onLoseFocus SanitizeTopic
            ]
            { onChange = TopicInputChange
            , label = Input.labelHidden ""
            , placeholder =
                text "Choose topic"
                    |> Input.placeholder []
                    |> Just
            , text = topicInput
            }
         ]
            |> row [ Element.paddingXY 0 3 ]
        )


viewReplyInput : Bool -> DisplayProfile -> ComposeModel -> UserInfo -> Element Msg
viewReplyInput a b c userInfo =
    viewComposePanel False (viewReplyLabel userInfo.chain) a b c userInfo


viewComposePanel : Bool -> Element Msg -> Bool -> DisplayProfile -> ComposeModel -> UserInfo -> Element Msg
viewComposePanel isCompose elem chainSwitchInProgress dProfile compose userInfo =
    let
        isMobile =
            dProfile == Mobile

        submitEnabled =
            not (String.isEmpty compose.body)
                && validTopic
                && not compose.inProgress

        validTopic =
            True

        topButton txt val =
            let
                active =
                    val == compose.preview
            in
            Input.button
                [ padding 10
                , Background.color orange
                    |> whenAttr active
                , Element.alignRight
                , Border.roundEach
                    { bottomLeft = 0
                    , topLeft = 5
                    , bottomRight = 0
                    , topRight = 5
                    }
                , hover
                    |> whenAttr (not active)
                , sansSerifFont
                , if active then
                    Font.color black

                  else
                    Font.color white
                , Font.bold
                ]
                { onPress = Just <| PreviewSet val
                , label = text txt
                }
    in
    [ [ [ topButton "Write" False
        , topButton "Preview" True
        ]
            |> row [ spacing 10, Element.paddingXY 10 0 ]
      , elem
            |> when (not isMobile)
      ]
        |> row [ width fill, spaceEvenly ]
    , [ View.Common.viewInstructions chainSwitchInProgress dProfile userInfo
      , Input.text
            [ width fill
            , View.Attrs.whiteGlowAttributeSmall
            ]
            { onChange = ComposeTitleChange
            , label = Input.labelHidden ""
            , placeholder =
                "Title (Optional)"
                    |> text
                    |> Input.placeholder []
                    |> Just
            , text = compose.title
            }
            |> when isCompose
      , viewMarkdown dProfile
            compose
            (if isCompose then
                px 450

             else
                px 200
            )
            |> el
                [ width fill
                , compose.error
                    |> whenJust
                        ((\txt ->
                            [ text txt
                            , Input.button [ hover ]
                                { onPress = Just CloseComposeError
                                , label = View.Img.close 25 black
                                }
                            ]
                         )
                            >> row
                                [ Background.color white
                                , slightRound
                                , padding 10
                                , spacing 10
                                , Font.color black
                                ]
                            >> el
                                [ padding 10
                                , Element.alignBottom
                                , Element.alignRight
                                ]
                        )
                    |> Element.inFront
                ]
      , [ viewBurnAmountUX compose.dollar
        , [ View.Common.cancel ComposeClose
                |> el [ Font.color black ]
                |> when (not isCompose)
          , Input.button
                [ Background.color Theme.darkGreen
                , Font.bold
                , Font.size 25
                , Element.alignRight
                , View.Attrs.roundBorder
                , if submitEnabled then
                    hover

                  else
                    View.Attrs.notAllowed
                , sansSerifFont
                , width <| px 100
                , height <| px 50
                ]
                { onPress =
                    if submitEnabled then
                        Just SubmitDraft

                    else
                        Nothing
                , label =
                    if compose.inProgress then
                        View.Common.spinner 20 white
                            |> el [ centerX, centerY ]

                    else
                        text "Submit"
                            |> el [ centerX, centerY ]
                }
          ]
            |> row [ Element.alignRight, spacing 20 ]
        ]
            |> row [ width fill ]
      ]
        |> column
            [ width fill
            , spacing 10
            , padding 10
            , roundBorder
            , Background.color orange
            ]
    ]
        |> column
            [ height fill
            , width fill
            ]


viewReplyLabel : Chain -> Element msg
viewReplyLabel chain =
    [ [ View.Img.replyArrow 25 orange
      , "Reply with"
            |> text
            |> el [ Font.color orange ]
      ]
        |> row [ spacing 10 ]
    , View.Common.chain chain
        |> el
            [ Background.color white
            , View.Attrs.roundBorder
            , padding 5
            , Font.color black
            ]
    ]
        |> row [ spacing 10, Element.moveUp 5 ]


viewBurnAmountUX : String -> Element Msg
viewBurnAmountUX amountInput =
    [ [ text "A higher burn means more visibility!" ]
        |> paragraph
            [ Font.size 14
            , spacing 3
            , Font.color white
            , Font.italic
            , Font.center
            , width fill
            ]
    , [ View.Img.dollar 26 white
      , Input.text
            [ View.Attrs.whiteGlowAttributeSmall
            , Background.color <| Element.rgb 0 0 0
            , Font.color white
            , width <| px 60
            , height <| px 34
            , padding 3
            , Font.size 26
            ]
            { onChange = ComposeDollarChange
            , label = Input.labelHidden ""
            , placeholder = Just <| Input.placeholder [] <| text "0.00"
            , text = amountInput
            }
      ]
        |> row [ spacing 5 ]
    ]
        |> row
            [ spacing 5
            , padding 5
            , Background.color <| Element.rgb 0.4 0.2 0.2
            , roundBorder
            , View.Attrs.cappedWidth 300
            ]


viewMarkdown : DisplayProfile -> ComposeModel -> Element.Length -> Element Msg
viewMarkdown dProfile compose heightLen =
    if compose.preview then
        (if String.isEmpty compose.body then
            text "Nothing to preview"

         else
            compose.body
                |> View.Markdown.renderString dProfile
        )
            |> el
                [ width fill
                , height heightLen
                , Element.scrollbarY
                , Font.color white
                , padding 10
                , Background.color black
                ]
            |> List.singleton
            |> column [ width fill, height fill ]

    else
        Input.multiline
            [ width fill
            , height heightLen
            , Element.scrollbarY
            , Font.color white
            , Border.width 0
            , Border.rounded 0
            , Background.color black
            ]
            { onChange = Types.ComposeBodyChange
            , label = Input.labelHidden ""
            , placeholder =
                "What do you want to say?"
                    |> text
                    |> Input.placeholder []
                    |> Just
            , text = compose.body
            , spellcheck = True
            }
            |> el
                [ Html.Attributes.class "multiline"
                    |> Element.htmlAttribute

                --, Element.scrollbarY
                , height fill
                , width fill
                ]
