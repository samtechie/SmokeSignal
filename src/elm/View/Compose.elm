module View.Compose exposing (view)

import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Helpers.Element exposing (DisplayProfile(..), black, white)
import Html.Attributes
import Maybe.Extra exposing (unwrap)
import Misc
import Theme exposing (orange)
import TokenValue
import Types exposing (..)
import View.Attrs exposing (hover, sansSerifFont, slightRound, whiteGlowAttributeSmall)
import View.Common exposing (when, whenAttr, whenJust, wrapModal)
import View.Img
import View.Markdown
import Wallet


view : Model -> Element Msg
view model =
    let
        onboard =
            model.wallet
                |> Wallet.userInfo
                |> unwrap True
                    (\_ ->
                        --(userInfo.chain /= XDai || TokenValue.isZero userInfo.balance)
                        --|| not model.hasOnboarded
                        not model.hasOnboarded
                    )
    in
    (if onboard then
        viewOnboarding model

     else
        model.wallet
            |> Wallet.userInfo
            |> whenJust (viewBox model)
    )
        |> (if model.dProfile == Mobile then
                identity

            else
                wrapModal ComposeClose
           )


viewOnboarding : Model -> Element Msg
viewOnboarding model =
    let
        step1 =
            not (model.wallet == Types.NoneDetected)

        step2 =
            Wallet.isActive model.wallet

        step3 =
            model.wallet
                |> Wallet.userInfo
                |> unwrap False (.chain >> (==) XDai)

        step4 =
            model.wallet
                |> Wallet.userInfo
                |> unwrap False
                    (\userInfo ->
                        userInfo.chain
                            == XDai
                            && (userInfo.balance |> TokenValue.isZero |> not)
                    )
    in
    [ [ text "To post or interact with SmokeSignal, you'll need a crypto identity:" ]
        |> paragraph [ Font.center, Font.size 22 ]
    , [ [ text "Install and setup "
        , Element.newTabLink
            [ Font.color Theme.orange, hover, Font.bold ]
            { url = "https://metamask.io/"
            , label = text "MetaMask"
            }
        ]
            |> viewCheck step1 False
      , [ text "Connect wallet"
        ]
            |> viewCheck step2 (model.wallet == Connecting)
            |> (\elem ->
                    if step1 && not step2 then
                        Input.button [ hover |> whenAttr (not <| model.wallet == Connecting) ]
                            { onPress =
                                if model.wallet == Connecting then
                                    Nothing

                                else
                                    Just Types.ConnectToWeb3
                            , label = elem
                            }

                    else if step2 then
                        elem

                    else
                        el [ View.Attrs.fade ] elem
               )
      , [ text "Enable xDai support"
        ]
            |> viewCheck
                step3
                model.chainSwitchInProgress
            |> (\elem ->
                    if step1 && step2 && not step3 then
                        Input.button [ hover |> (whenAttr <| not model.chainSwitchInProgress) ]
                            { onPress =
                                if model.chainSwitchInProgress then
                                    Nothing

                                else
                                    Just Types.XDaiImport
                            , label = elem
                            }

                    else if step3 then
                        elem

                    else
                        el [ View.Attrs.fade ] elem
               )
      , [ text "Get free xDai"
        ]
            |> viewCheck
                step4
                model.faucetInProgress
            |> (\elem ->
                    if step1 && step2 && step3 && not step4 then
                        Input.button [ hover |> (whenAttr <| not model.faucetInProgress) ]
                            { onPress =
                                if model.faucetInProgress then
                                    Nothing

                                else
                                    Just SubmitFaucet
                            , label = elem
                            }

                    else if step4 then
                        elem

                    else
                        el [ View.Attrs.fade ] elem
               )
      ]
        |> column [ spacing 20, width fill ]
    , View.Common.cancel ComposeClose
        |> el [ Element.alignRight ]
    ]
        |> column
            [ padding 30
            , spacing 30
            , whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            , width fill
            , centerY
                |> View.Common.whenAttr (model.dProfile == Mobile)
            ]


viewCheck : Bool -> Bool -> List (Element Msg) -> Element Msg
viewCheck tick inProg elems =
    [ (if tick then
        View.Img.tick 25 black

       else if inProg then
        View.Common.spinner 20 black

       else
        Element.none
      )
        |> el
            [ centerX
            , centerY
            ]
        |> el
            [ width <| px 30
            , height <| px 30
            , Background.color white
            , whiteGlowAttributeSmall

            --, hover
            ]
    , elems
        |> paragraph []
    ]
        |> row [ width fill, spacing 20 ]


viewBox : Model -> UserInfo -> Element Msg
viewBox model userInfo =
    let
        isMobile =
            model.dProfile == Helpers.Element.Mobile

        validTopic =
            case model.compose.context of
                Reply _ ->
                    True

                TopLevel _ ->
                    model.topicInput
                        |> Misc.validateTopic
                        |> (/=) Nothing

        submitEnabled =
            not (String.isEmpty model.compose.body)
                && validTopic

        whitespace =
            if isMobile then
                10

            else
                20
    in
    [ "Compose"
        |> text
        |> el [ sansSerifFont, Font.color white, centerX ]
        |> el
            [ View.Attrs.sansSerifFont
            , padding 10
            , slightRound
            , Background.color Theme.orange
            , Font.bold
            , Font.color white
            , Font.size 20
            , width fill
            ]
    , [ [ View.Common.viewChain userInfo.chain
            |> el
                [ Background.color white
                , View.Attrs.roundBorder
                , padding 5
                , Element.alignRight
                ]
            |> when (not isMobile)
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
            , text = model.compose.title
            }
        , [ case model.compose.context of
                TopLevel _ ->
                    Input.text
                        [ Background.color white
                        , width <| px 250
                        , Border.roundEach
                            { bottomLeft = 0
                            , topLeft = 0
                            , bottomRight = 5
                            , topRight = 5
                            }
                        , spacing 0
                        , Element.Events.onLoseFocus SanitizeTopic
                        ]
                        { onChange = TopicInputChange
                        , label =
                            "Topic"
                                |> text
                                |> el [ centerY ]
                                |> Input.labelLeft
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
                        , placeholder = Nothing
                        , text = model.topicInput
                        }

                Reply _ ->
                    [ View.Img.replyArrow 25 orange
                    , "Reply"
                        |> text
                        |> el [ Font.color orange ]
                    ]
                        |> row [ spacing 10 ]
          , viewBurnBox model.compose.donate model.compose.dollar
          ]
            |> (if isMobile then
                    column [ width fill, spacing 10 ]

                else
                    row [ width fill, spaceEvenly ]
               )
        ]
            |> column [ width fill, spacing 10, sansSerifFont ]
      , viewMarkdown model
      , model.compose.error
            |> whenJust
                (text
                    >> List.singleton
                    >> Element.paragraph
                        [ Background.color white
                        , Element.alignRight
                        , slightRound
                        , padding 10
                        , Font.color black
                        ]
                )
      , [ [ Input.checkbox
                [ width <| px 30
                , height <| px 30
                , Background.color white
                , whiteGlowAttributeSmall
                , hover
                ]
                { onChange = Types.PreviewSet
                , icon =
                    \checked ->
                        View.Img.tick 20 black
                            |> el
                                [ centerX
                                , centerY
                                ]
                            |> View.Common.when checked
                , checked = model.compose.preview
                , label = Input.labelHidden "Preview"
                }
          , "Preview"
                |> text
                |> el [ Font.color white ]
          ]
            |> row [ spacing 5 ]
            |> when isMobile
        , [ View.Common.cancel ComposeClose
                |> el [ Font.color white ]
          , Input.button
                [ padding 10
                , Background.color orange
                , Element.alignRight
                , View.Attrs.roundBorder
                , if submitEnabled then
                    hover

                  else
                    View.Attrs.style "cursor" "not-allowed"
                , sansSerifFont
                ]
                { onPress =
                    if submitEnabled then
                        Just SubmitDraft

                    else
                        Nothing
                , label =
                    if model.compose.inProgress then
                        View.Common.spinner 20 black
                            |> el [ centerX ]

                    else
                        text "Submit"
                }
          ]
            |> row [ Element.alignRight, spacing 10 ]
        ]
            |> row [ width fill ]
      ]
        |> column
            [ height fill
            , width fill
            , spacing whitespace
            , padding whitespace
            ]
    ]
        |> column
            [ if isMobile then
                width fill

              else
                width <| px 1000
            , height fill
            , Background.color black
            , whiteGlowAttributeSmall
                |> whenAttr (not isMobile)
            , View.Attrs.style "z-index" "2000"
            , sansSerifFont
            ]


viewBurnBox : Bool -> String -> Element Msg
viewBurnBox donate txt =
    [ [ View.Img.dollar 30 white
      , Input.text
            [ View.Attrs.whiteGlowAttributeSmall
            , Background.color white
            , width <| px 250
            ]
            { onChange = ComposeDollarChange
            , label = Input.labelHidden ""
            , placeholder =
                "Burn amount (USD)"
                    |> text
                    |> Input.placeholder []
                    |> Just
            , text = txt
            }
      ]
        |> row [ spacing 5 ]
    , [ Input.checkbox
            [ width <| px 30
            , height <| px 30
            , Background.color white
            , whiteGlowAttributeSmall
            , hover
            ]
            { onChange = Types.DonationCheckboxSet
            , icon =
                \checked ->
                    View.Img.tick 20 black
                        |> el
                            [ centerX
                            , centerY
                            ]
                        |> View.Common.when checked
            , checked = donate
            , label = Input.labelHidden "Donate an extra 1% to Foundry"
            }
      , [ text "Donate an extra 1% to "
        , Element.newTabLink
            [ Font.color Theme.orange, hover, Font.bold ]
            { url = "https://foundrydao.com/"
            , label = text "Foundry"
            }
        , text " so we can build more cool stuff!"
        ]
            |> Element.paragraph [ spacing 5, Font.color white ]
      ]
        |> row
            [ Font.size 15
            , spacing 10
            ]
    ]
        |> column [ spacing 10 ]


viewMarkdown : Model -> Element Msg
viewMarkdown model =
    let
        isMobile =
            model.dProfile == Helpers.Element.Mobile
    in
    if isMobile then
        if model.compose.preview then
            model.compose.body
                |> View.Markdown.renderString model.dProfile
                |> el
                    [ width fill
                    , height fill
                    , Element.scrollbarY
                    , whiteGlowAttributeSmall
                    , Font.color white
                    , padding 10
                    ]
                |> List.singleton
                |> column [ width fill, height fill ]

        else
            Input.multiline
                [ if isMobile then
                    width fill

                  else
                    width <| px 500
                , if isMobile then
                    height fill

                  else
                    height <| px 500
                , View.Attrs.whiteGlowAttributeSmall
                , Background.color black
                , Font.color white
                , height <| px 0
                ]
                { onChange = Types.ComposeBodyChange
                , label = Input.labelHidden ""
                , placeholder =
                    "What do you want to say?"
                        |> text
                        |> Input.placeholder []
                        |> Just
                , text = model.compose.body
                , spellcheck = True
                }
                |> el
                    [ Html.Attributes.class "multiline"
                        |> Element.htmlAttribute
                    , Element.scrollbarY
                    , height fill
                    , width fill
                    ]

    else
        [ Input.multiline
            [ width <| px 500
            , height <| px 500
            , View.Attrs.whiteGlowAttributeSmall
            , Background.color black
            , Font.color white
            ]
            { onChange = Types.ComposeBodyChange
            , label = Input.labelHidden ""
            , placeholder =
                "What do you want to say?"
                    |> text
                    |> Input.placeholder []
                    |> Just
            , text = model.compose.body
            , spellcheck = True
            }
        , model.compose.body
            |> View.Markdown.renderString model.dProfile
            |> el
                [ width <| px 500

                --, height fill
                , Element.scrollbarY
                , height <| px 500

                --, View.Attrs.style "min-height" "auto"
                , whiteGlowAttributeSmall
                , Font.color white
                , padding 10
                ]
        ]
            |> row
                [ height fill
                , width fill
                , spacing 30
                ]
