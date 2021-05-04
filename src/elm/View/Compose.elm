module View.Compose exposing (view)

import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Maybe.Extra
import Theme exposing (black, orange, white)
import Types exposing (..)
import View.Attrs exposing (hover, roundBorder, sansSerifFont, whiteGlowAttributeSmall)
import View.Common
import View.Img
import View.Markdown
import View.Post
import Wallet


view : Model -> Element Msg
view model =
    model.wallet
        |> Wallet.userInfo
        |> Maybe.Extra.unwrap
            (Input.button
                [ Background.color Theme.orange
                , padding 10
                , View.Attrs.roundBorder
                , hover
                , Font.color black
                , centerX
                , centerY
                ]
                { onPress = Just ConnectToWeb3
                , label =
                    if model.wallet == Connecting then
                        View.Common.spinner 20 black
                            |> el [ centerX ]

                    else
                        [ text "Connect wallet to compose post" ]
                            |> paragraph [ Font.center ]
                }
                |> el
                    [ Background.color black
                    , whiteGlowAttributeSmall
                    , height <| px 150
                    , width <| px 240
                    , padding 40
                    , centerX
                    ]
                |> el [ width fill, height fill, padding 20 ]
            )
            (View.Post.viewComposeInput model.topicInput
                model.chainSwitchInProgress
                model.dProfile
                model.compose
            )
