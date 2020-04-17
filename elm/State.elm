port module State exposing (init, subscriptions, update)

import Browser
import Browser.Events
import Browser.Navigation
import Common.Msg exposing (..)
import Common.Types exposing (..)
import ComposeUX.State as ComposeUX
import ComposeUX.Types as ComposeUX
import Config
import Contracts.Dai as Dai
import Contracts.SmokeSignal as SSContract
import Dict exposing (Dict)
import Eth
import Eth.Decode
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry
import Eth.Types exposing (Address, TxHash)
import Eth.Utils
import Helpers.Element as EH
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import MaybeDebugLog exposing (maybeDebugLog)
import Post exposing (Post)
import Routing exposing (Route)
import Task
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import Wallet


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Routing.urlToRoute url

        ( wallet, walletNotices ) =
            if flags.networkId == 0 then
                ( Wallet.NoneDetected
                , [ UN.noWeb3Provider ]
                )

            else
                ( Wallet.OnlyNetwork <| Eth.Net.toNetworkId flags.networkId
                , []
                )

        txSentry =
            TxSentry.init
                ( txOut, txIn )
                TxSentryMsg
                Config.httpProviderUrl

        ( initEventSentry, initEventSentryCmd ) =
            EventSentry.init EventSentryMsg Config.httpProviderUrl

        ( eventSentry, secondEventSentryCmd, _ ) =
            fetchPostsFromBlockrangeCmd
                (Eth.Types.BlockNum Config.startScanBlock)
                Eth.Types.LatestBlock
                initEventSentry
    in
    { navKey = key
    , route = Routing.InitialBlank
    , wallet = wallet
    , now = Time.millisToPosix flags.nowInMillis
    , dProfile = EH.screenWidthToDisplayProfile flags.width
    , txSentry = txSentry
    , eventSentry = eventSentry
    , posts = Dict.empty
    , replies = []
    , mode = Home
    , showHalfComposeUX = False
    , composeUXModel = ComposeUX.init wallet
    , blockTimes = Dict.empty
    , showAddress = Nothing
    , userNotices = walletNotices
    , trackedTxs = Dict.empty
    }
        |> gotoRoute route
        |> Tuple.mapSecond
            (\routeCmd ->
                Cmd.batch
                    [ initEventSentryCmd
                    , secondEventSentryCmd
                    , routeCmd
                    ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg prevModel =
    case msg of
        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            Browser.Navigation.pushUrl prevModel.navKey (Url.toString url)

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( prevModel, cmd )

        UrlChanged url ->
            prevModel |> updateFromPageRoute (url |> Routing.urlToRoute)

        Tick newTime ->
            ( { prevModel | now = newTime }, Cmd.none )

        Resize width _ ->
            ( { prevModel
                | dProfile =
                    EH.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        EveryFewSeconds ->
            ( prevModel
            , Wallet.userInfo prevModel.wallet
                |> Maybe.map
                    (\userInfo ->
                        fetchDaiBalanceAndAllowanceCmd userInfo.address
                    )
                |> Maybe.withDefault Cmd.none
            )

        CheckTrackedTxsStatus ->
            ( prevModel
            , prevModel.trackedTxs
                |> Dict.filter
                    (\_ trackedTx ->
                        trackedTx.status /= Mined
                    )
                |> Dict.keys
                |> List.map Eth.Utils.unsafeToTxHash
                |> List.map (Eth.getTxReceipt Config.httpProviderUrl)
                |> List.map (Task.attempt TrackedTxStatusResult)
                |> Cmd.batch
            )

        TrackedTxStatusResult txReceiptResult ->
            case txReceiptResult of
                Err errStr ->
                    let
                        _ =
                            Debug.log "txReceipt poll err. Important to catch? Currently ignoring." errStr
                    in
                    -- Hasn't yet been mined; make no change
                    ( prevModel, Cmd.none )

                Ok txReceipt ->
                    case Dict.get (Eth.Utils.txHashToString txReceipt.hash) prevModel.trackedTxs of
                        Nothing ->
                            -- no matching trackedTx; ignore
                            ( prevModel, Cmd.none )

                        Just trackedTx ->
                            case trackedTx.status of
                                Mined ->
                                    -- Already updated; ignore
                                    ( prevModel, Cmd.none )

                                _ ->
                                    ( prevModel
                                        |> updateTrackedTxStatus txReceipt.hash Mined
                                    , Cmd.none
                                    )

        WalletStatus walletSentryResult ->
            case walletSentryResult of
                Ok walletSentry ->
                    let
                        ( newWallet, cmd ) =
                            case walletSentry.account of
                                Just newAddress ->
                                    if (prevModel.wallet |> Wallet.userInfo |> Maybe.map .address) == Just newAddress then
                                        ( prevModel.wallet
                                        , Cmd.none
                                        )

                                    else
                                        ( Wallet.Active <|
                                            UserInfo
                                                walletSentry.networkId
                                                newAddress
                                                Nothing
                                                Nothing
                                        , fetchDaiBalanceAndAllowanceCmd newAddress
                                        )

                                Nothing ->
                                    ( Wallet.OnlyNetwork walletSentry.networkId
                                    , Cmd.none
                                    )
                    in
                    { prevModel
                        | wallet = newWallet
                    }
                        |> sendMsgDown (UpdateWallet newWallet)

                Err errStr ->
                    ( prevModel |> addUserNotice (UN.walletError errStr)
                    , Cmd.none
                    )

        TxSentryMsg subMsg ->
            let
                ( newTxSentry, subCmd ) =
                    TxSentry.update subMsg prevModel.txSentry
            in
            ( { prevModel | txSentry = newTxSentry }, subCmd )

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        prevModel.eventSentry
            in
            ( { prevModel
                | eventSentry =
                    newEventSentry
              }
            , cmd
            )

        PostLogReceived log ->
            let
                decodedEventLog =
                    Eth.Decode.event SSContract.messageBurnDecoder log
            in
            case decodedEventLog.returnData of
                Err err ->
                    ( prevModel |> addUserNotice (UN.eventDecodeError err)
                    , Cmd.none
                    )

                Ok ssPost ->
                    ( prevModel
                        |> addPost log.blockNumber
                            (SSContract.fromMessageBurn
                                log.blockNumber
                                ssPost
                            )
                        |> updateTrackedTxStatus
                            log.transactionHash
                            Mined
                    , getBlockTimeIfNeededCmd prevModel.blockTimes log.blockNumber
                    )

        BalanceFetched address fetchResult ->
            let
                maybeCurrentAddress =
                    Wallet.userInfo prevModel.wallet
                        |> Maybe.map .address
            in
            if maybeCurrentAddress /= Just address then
                ( prevModel, Cmd.none )

            else
                case fetchResult of
                    Ok balance ->
                        let
                            newWallet =
                                prevModel.wallet |> Wallet.withFetchedBalance balance
                        in
                        { prevModel
                            | wallet = newWallet
                        }
                            |> sendMsgDown (UpdateWallet newWallet)

                    Err httpErr ->
                        ( prevModel
                            |> addUserNotice (UN.web3FetchError "DAI balance" httpErr)
                        , Cmd.none
                        )

        AllowanceFetched address fetchResult ->
            let
                maybeCurrentAddress =
                    Wallet.userInfo prevModel.wallet
                        |> Maybe.map .address
            in
            if maybeCurrentAddress /= Just address then
                ( prevModel, Cmd.none )

            else
                case fetchResult of
                    Ok allowance ->
                        let
                            isUnlocked =
                                TokenValue.isMaxTokenValue allowance

                            newWallet =
                                prevModel.wallet |> Wallet.withIsUnlocked isUnlocked
                        in
                        { prevModel
                            | wallet =
                                newWallet
                        }
                            |> sendMsgDown (UpdateWallet newWallet)

                    Err httpErr ->
                        ( prevModel
                            |> addUserNotice (UN.web3FetchError "DAI unlock status" httpErr)
                        , Cmd.none
                        )

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err httpErr ->
                    ( prevModel
                        |> addUserNotice (UN.web3FetchError "block time" httpErr)
                    , Cmd.none
                    )

                Ok time ->
                    ( { prevModel
                        | blockTimes =
                            prevModel.blockTimes
                                |> Dict.insert blocknum time
                      }
                    , Cmd.none
                    )

        DismissNotice id ->
            ( { prevModel
                | userNotices =
                    prevModel.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        ComposeUXMsg composeUXMsg ->
            let
                updateResult =
                    prevModel.composeUXModel
                        |> ComposeUX.update composeUXMsg
            in
            ( { prevModel
                | composeUXModel =
                    updateResult.newModel
              }
            , Cmd.map ComposeUXMsg updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps

        TxSigned txInfo txHashResult ->
            case txHashResult of
                Ok txHash ->
                    let
                        maybeMsgDown =
                            case txInfo of
                                PostTx draft ->
                                    Just <| PostSigned draft

                                _ ->
                                    Nothing
                    in
                    ( prevModel
                        |> addTrackedTx txHash txInfo
                    , Cmd.none
                    )
                        |> (maybeMsgDown
                                |> Maybe.map withMsgDown
                                |> Maybe.withDefault identity
                           )

                Err errStr ->
                    ( prevModel
                        |> addUserNotice
                            (UN.web3SigError
                                (txInfoToNameStr txInfo)
                                errStr
                            )
                    , Cmd.none
                    )

        TxMined txInfo txReceiptResult ->
            case txReceiptResult of
                Ok txReceipt ->
                    ( prevModel
                        |> updateTrackedTxStatus txReceipt.hash Mined
                    , Cmd.none
                    )

                Err errStr ->
                    ( prevModel
                        |> updateTrackedTxStatusByTxInfo txInfo (Failed errStr)
                    , Cmd.none
                    )

        NoOp ->
            ( prevModel, Cmd.none )

        ClickHappened ->
            ( { prevModel
                | showAddress = Nothing
              }
            , Cmd.none
            )


handleMsgUp : MsgUp -> Model -> ( Model, Cmd Msg )
handleMsgUp msgUp prevModel =
    case msgUp of
        GotoRoute route ->
            prevModel
                |> gotoRoute route
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Browser.Navigation.pushUrl
                                prevModel.navKey
                                (Routing.routeToString route)
                            ]
                    )

        ConnectToWeb3 ->
            case prevModel.wallet of
                Wallet.NoneDetected ->
                    ( prevModel |> addUserNotice UN.cantConnectNoWeb3
                    , Cmd.none
                    )

                _ ->
                    ( prevModel
                    , connectToWeb3 ()
                    )

        ShowOrHideAddress phaceId ->
            ( { prevModel
                | showAddress =
                    if prevModel.showAddress == Just phaceId then
                        Nothing

                    else
                        Just phaceId
              }
            , Cmd.none
            )

        ShowHalfComposeUX flag ->
            ( { prevModel
                | showHalfComposeUX = flag
              }
            , Cmd.none
            )

        AddUserNotice userNotice ->
            ( prevModel |> addUserNotice userNotice
            , Cmd.none
            )

        UnlockDai ->
            let
                txParams =
                    Dai.unlockDaiCall
                        |> Eth.toSend

                listeners =
                    { onMined = Just ( TxMined UnlockTx, Nothing )
                    , onSign = Just <| TxSigned UnlockTx
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )

        SubmitPost postDraft ->
            let
                txParams =
                    postDraft
                        |> Post.encodeDraft
                        |> SSContract.burnEncodedPost
                        |> Eth.toSend

                listeners =
                    { onMined = Just ( TxMined <| PostTx postDraft, Nothing )
                    , onSign = Just <| TxSigned <| PostTx postDraft
                    , onBroadcast = Nothing
                    }

                ( txSentry, cmd ) =
                    TxSentry.customSend prevModel.txSentry listeners txParams
            in
            ( { prevModel
                | txSentry = txSentry
              }
            , cmd
            )


addTrackedTx : TxHash -> TxInfo -> Model -> Model
addTrackedTx txHash txInfo prevModel =
    { prevModel
        | trackedTxs =
            prevModel.trackedTxs
                |> Dict.insert
                    (Eth.Utils.txHashToString txHash)
                    (TrackedTx
                        txInfo
                        Mining
                    )
    }


withMsgUp : MsgUp -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withMsgUp msgUp ( prevModel, prevCmd ) =
    handleMsgUp msgUp prevModel
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ prevCmd, newCmd ]
            )


handleMsgUps : List MsgUp -> Model -> ( Model, Cmd Msg )
handleMsgUps msgUps prevModel =
    List.foldl
        withMsgUp
        ( prevModel, Cmd.none )
        msgUps


withMsgUps : List MsgUp -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withMsgUps msgUps ( prevModel, prevCmd ) =
    handleMsgUps msgUps prevModel
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ prevCmd, newCmd ]
            )


sendMsgDown : MsgDown -> Model -> ( Model, Cmd Msg )
sendMsgDown msgDown prevModel =
    let
        updateResult =
            prevModel.composeUXModel
                |> ComposeUX.handleMsgDown msgDown

        ( newMainModel, cmd1 ) =
            { prevModel
                | composeUXModel = updateResult.newModel
            }
                |> handleMsgUps updateResult.msgUps
    in
    ( newMainModel
    , Cmd.batch
        [ cmd1
        , Cmd.map ComposeUXMsg updateResult.cmd
        ]
    )


withMsgDown : MsgDown -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withMsgDown msgDown ( prevModel, prevCmd ) =
    prevModel
        |> sendMsgDown msgDown
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ newCmd, prevCmd ]
            )


updateFromPageRoute : Route -> Model -> ( Model, Cmd Msg )
updateFromPageRoute route model =
    if model.route == route then
        ( model
        , Cmd.none
        )

    else
        gotoRoute route model


gotoRoute : Route -> Model -> ( Model, Cmd Msg )
gotoRoute route prevModel =
    ( case routeToMode route of
        Ok mode ->
            { prevModel
                | route = route
                , mode = mode
            }

        Err errStr ->
            { prevModel
                | route = route
            }
                |> addUserNotice UN.routeNotFound
    , Cmd.none
    )


routeToMode : Route -> Result String Mode
routeToMode route =
    case route of
        Routing.InitialBlank ->
            Ok Home

        Routing.Home ->
            Ok Home

        Routing.ViewAll ->
            Ok ViewAll

        Routing.ViewPost postId ->
            Ok <| ViewPost postId

        Routing.ViewTopic topic ->
            Ok <| ViewTopic topic

        Routing.NotFound err ->
            Err err


addPost : Int -> Post -> Model -> Model
addPost blockNumber post prevModel =
    let
        alreadyHavePost =
            prevModel.posts
                |> Dict.get blockNumber
                |> Maybe.map
                    (List.any
                        (\listedPost ->
                            listedPost.postId == post.postId
                        )
                    )
                |> Maybe.withDefault False
    in
    if alreadyHavePost then
        prevModel

    else
        { prevModel
            | posts =
                prevModel.posts
                    |> Dict.update blockNumber
                        (\maybePostsForBlock ->
                            Just <|
                                case maybePostsForBlock of
                                    Nothing ->
                                        [ post ]

                                    Just posts ->
                                        List.append posts [ post ]
                        )
            , replies =
                List.append
                    prevModel.replies
                    (case post.metadata |> Result.toMaybe |> Maybe.andThen .replyTo of
                        Just replyTo ->
                            [ { from = post.postId
                              , to = replyTo
                              }
                            ]

                        Nothing ->
                            []
                    )
        }


getBlockTimeIfNeededCmd : Dict Int Time.Posix -> Int -> Cmd Msg
getBlockTimeIfNeededCmd blockTimes blockNumber =
    if Dict.get blockNumber blockTimes == Nothing then
        getBlockTimeCmd blockNumber

    else
        Cmd.none


fetchPostsFromBlockrangeCmd : Eth.Types.BlockId -> Eth.Types.BlockId -> EventSentry Msg -> ( EventSentry Msg, Cmd Msg, EventSentry.Ref )
fetchPostsFromBlockrangeCmd from to sentry =
    EventSentry.watch
        PostLogReceived
        sentry
    <|
        SSContract.messageBurnEventFilter
            from
            to
            Nothing
            Nothing


fetchDaiBalanceAndAllowanceCmd : Address -> Cmd Msg
fetchDaiBalanceAndAllowanceCmd address =
    Cmd.batch
        [ Dai.getAllowanceCmd address (AllowanceFetched address)
        , Dai.getBalanceCmd address (BalanceFetched address)
        ]


getBlockTimeCmd : Int -> Cmd Msg
getBlockTimeCmd blocknum =
    Eth.getBlock
        Config.httpProviderUrl
        blocknum
        |> Task.map .timestamp
        |> Task.attempt (BlockTimeFetched blocknum)


addUserNotice : UserNotice -> Model -> Model
addUserNotice notice model =
    model
        |> addUserNotices [ notice ]


addUserNotices : List UserNotice -> Model -> Model
addUserNotices notices model =
    { model
        | userNotices =
            List.append
                model.userNotices
                notices
                |> List.Extra.uniqueBy .uniqueLabel
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        , Time.every 2500 (always EveryFewSeconds)
        , Time.every 5000 (always CheckTrackedTxsStatus)
        , walletSentryPort
            (WalletSentry.decodeToMsg
                (WalletStatus << Err)
                (WalletStatus << Ok)
            )
        , TxSentry.listen model.txSentry
        , Browser.Events.onResize Resize
        ]


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port connectToWeb3 : () -> Cmd msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg
