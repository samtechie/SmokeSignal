module App exposing (main)

import Browser.Events
import Browser.Hashbang
import Browser.Navigation
import Contracts.SmokeSignal
import Dict
import Eth.Defaults
import Eth.Net
import Eth.Sentry.Event
import Eth.Sentry.Tx
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Utils
import Helpers.Element
import Misc exposing (tryRouteToView)
import Ports
import Post
import Routing
import Time
import Types exposing (Flags, Model, Msg)
import Update exposing (update)
import Url exposing (Url)
import UserNotice as UN
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.Hashbang.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = Routing.urlToRoute >> RouteChanged
        }


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        config =
            { smokeSignalContractAddress = Eth.Utils.unsafeToAddress flags.smokeSignalContractAddress
            , httpProviderUrl = flags.httpProviderUrl
            , startScanBlock = flags.startScanBlock
            }

        route =
            Routing.urlToRoute url

        ( view, routingUserNotices ) =
            case tryRouteToView route of
                Ok v ->
                    ( v, [] )

                Err err ->
                    ( ViewHome
                    , [ UN.routeNotFound <| Just err ]
                    )

        ( wallet, walletNotices ) =
            if flags.networkId == 0 then
                ( Types.NoneDetected
                , [ UN.noWeb3Provider ]
                )

            else
                ( Types.OnlyNetwork <| Eth.Net.toNetworkId flags.networkId
                , []
                )

        txSentry =
            Eth.Sentry.Tx.init
                ( Ports.txOut, Ports.txIn )
                Types.TxSentryMsg
                config.httpProviderUrl

        ( initEventSentry, initEventSentryCmd ) =
            Eth.Sentry.Event.init Types.EventSentryMsg config.httpProviderUrl

        ( eventSentry, secondEventSentryCmd, _ ) =
            Contracts.SmokeSignal.messageBurnEventFilter
                config.smokeSignalContractAddress
                (Eth.Types.BlockNum config.startScanBlock)
                Eth.Types.LatestBlock
                Nothing
                Nothing
                |> Eth.Sentry.Event.watch
                    Types.PostLogReceived
                    initEventSentry

        now =
            Time.millisToPosix flags.nowInMillis

        model =
            Misc.emptyModel key
    in
    ( { model
        | basePath = flags.basePath
        , view = view
        , wallet = wallet
        , now = now
        , dProfile = Helpers.Element.screenWidthToDisplayProfile flags.width
        , txSentry = txSentry
        , eventSentry = eventSentry
        , userNotices = walletNotices
        , cookieConsentGranted = flags.cookieConsent
        , newUserModal = flags.newUser
        , config = config
      }
    , Cmd.batch
        [ initEventSentryCmd
        , secondEventSentryCmd
        , Misc.fetchEthPriceCmd config
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Types.Tick
        , Time.every 2000 (always Types.ChangeDemoPhaceSrc)
        , Time.every 2500 (always Types.EveryFewSeconds)
        , Time.every 5000 (always Types.CheckTrackedTxsStatus)
        , Ports.walletSentryPort
            (Eth.Sentry.Wallet.decodeToMsg
                (Types.WalletStatus << Err)
                (Types.WalletStatus << Ok)
            )
        , Eth.Sentry.Tx.listen model.txSentry
        , Browser.Events.onResize Types.Resize

        --, Sub.map ComposeUXMsg ComposeUX.subscriptions
        ]
