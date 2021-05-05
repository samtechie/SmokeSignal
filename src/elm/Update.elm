module Update exposing (update)

import Array
import Browser.Dom
import Chain
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator
import Dict exposing (Dict)
import Eth
import Eth.Types exposing (TxReceipt)
import Eth.Utils
import GTag exposing (GTagData, gTagOut, gTagOutOnlyOnLabelOrValueChange, gTagOutOnlyOnceForEvent)
import Http
import Json.Decode
import List.Extra
import Maybe.Extra exposing (unwrap)
import Misc exposing (emptyComposeModel, postIdToKey, sortTypeToString)
import Ports
import Post
import Random
import Result.Extra exposing (unpack)
import Routing exposing (viewUrlToPathString)
import Sentry
import Set
import Task
import Time
import TokenValue exposing (TokenValue)
import Tracking
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet exposing (userInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ensureUserInfo fn =
            model.wallet
                |> Wallet.userInfo
                |> unwrap ( model, Ports.log "Missing wallet" ) fn
    in
    case msg of
        RouteChanged route ->
            handleRoute model route

        Tick newTime ->
            ( { model | now = newTime }, Cmd.none )

        PreviewSet val ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | preview = val })
              }
            , Cmd.none
            )

        Resize width _ ->
            ( { model
                | dProfile =
                    Misc.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        ToggleTrackedTxs ->
            ( { model
                | showExpandedTrackedTxs = not model.showExpandedTrackedTxs
              }
            , Cmd.none
            )

        PostResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\err ->
                                case err of
                                    Types.UserRejected ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "post tx rejected"
                                                    ("tx rejected" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( { model
                                            | compose =
                                                model.compose
                                                    |> (\r ->
                                                            { r
                                                                | inProgress = False
                                                                , error =
                                                                    Just "Your transaction was cancelled."
                                                            }
                                                       )
                                          }
                                        , gtagCmd
                                        )

                                    Types.OtherErr e ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "post tx error"
                                                    ("tx error" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( { model
                                            | compose =
                                                model.compose
                                                    |> (\r ->
                                                            { r
                                                                | inProgress = False
                                                                , error =
                                                                    Just "There has been a problem."
                                                            }
                                                       )
                                          }
                                        , [ logString "PostResponse" e
                                          , gtagCmd
                                          ]
                                            |> Cmd.batch
                                        )
                            )
                            (\txHash ->
                                let
                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "post tx confirmed"
                                            ("tx confirmed" |> Just)
                                            (Eth.Utils.txHashToString txHash
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | inProgress = False
                                                        , modal = False
                                                        , reply = False
                                                    }
                                               )
                                    , showExpandedTrackedTxs = True
                                    , userNotices =
                                        model.userNotices
                                            |> List.append [ UN.notify "Your transaction is mining." ]
                                    , trackedTxs =
                                        model.trackedTxs
                                            |> Dict.insert (Eth.Utils.txHashToString txHash)
                                                { txHash = txHash
                                                , txInfo = PostTx txHash
                                                , status = Mining
                                                , chain = userInfo.chain
                                                }
                                    , gtagHistory = newGtagHistory
                                  }
                                , [ gtagCmd
                                  , Tracking.postSubmitted
                                  ]
                                    |> Cmd.batch
                                )
                            )
                )

        ChainSwitchResponse res ->
            res
                |> unpack
                    (\err ->
                        case err of
                            Types.UserRejected ->
                                ( { model
                                    | chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            Types.OtherErr e ->
                                ( { model
                                    | chainSwitchInProgress = False
                                  }
                                , logString "ChainSwitchResponse" e
                                )
                    )
                    (\() ->
                        ( -- Wait for WalletResponse to update model.chainSwitchInProgress
                          model
                        , Cmd.none
                        )
                    )

        BurnOrTipResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\err ->
                                case err of
                                    Types.UserRejected ->
                                        -- this could happen if the user clicks twice, accepts the first, then rejects the second.
                                        -- Even though a tx is mining, the dapp will tell the user it has failed.
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "burn/tip tx rejected"
                                                    ("tx rejected" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( { model
                                            | maybeBurnOrTipUX =
                                                model.maybeBurnOrTipUX
                                                    |> Maybe.map
                                                        (\r ->
                                                            { r
                                                                | inProgress = False
                                                                , error =
                                                                    Just "Your transaction was cancelled."
                                                            }
                                                        )
                                          }
                                        , gtagCmd
                                        )

                                    Types.OtherErr e ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "burn/tip tx error"
                                                    ("tx error" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( { model
                                            | maybeBurnOrTipUX =
                                                model.maybeBurnOrTipUX
                                                    |> Maybe.map
                                                        (\r ->
                                                            { r
                                                                | inProgress = False
                                                                , error =
                                                                    Just "There has been a problem."
                                                            }
                                                        )
                                          }
                                        , [ logString "BurnOrTipResponse" e
                                          , gtagCmd
                                          ]
                                            |> Cmd.batch
                                        )
                            )
                            (\txHash ->
                                let
                                    trackedTx =
                                        model.maybeBurnOrTipUX
                                            |> Maybe.map
                                                (\burnOrTipUX ->
                                                    { txHash = txHash
                                                    , txInfo =
                                                        case burnOrTipUX.burnOrTip of
                                                            Tip ->
                                                                TipTx burnOrTipUX.id

                                                            Burn ->
                                                                BurnTx burnOrTipUX.id
                                                    , status = Mining
                                                    , chain = userInfo.chain
                                                    }
                                                )

                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "burn/tip tx confirmed"
                                            ("tx confirmed" |> Just)
                                            (Eth.Utils.txHashToString txHash
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | maybeBurnOrTipUX = Nothing
                                    , userNotices =
                                        model.userNotices
                                            |> List.append [ UN.notify "Your transaction is mining." ]
                                    , trackedTxs =
                                        model.trackedTxs
                                            |> (trackedTx
                                                    |> unwrap identity
                                                        (Dict.insert
                                                            (Eth.Utils.txHashToString txHash)
                                                        )
                                               )
                                    , gtagHistory = newGtagHistory
                                  }
                                , gtagCmd
                                )
                            )
                )

        CheckTrackedTxsStatus ->
            ( model
            , model.trackedTxs
                |> Dict.values
                |> List.filter
                    (\trackedTx ->
                        trackedTx.status == Mining
                    )
                |> List.map
                    (\tx ->
                        Misc.getTxReceipt
                            (Chain.getProviderUrl tx.chain model.config)
                            tx.txHash
                            |> Task.attempt TrackedTxStatusResult
                    )
                |> Cmd.batch
            )

        TrackedTxStatusResult res ->
            case res of
                Err err ->
                    ( model
                    , logHttpError "TrackedTxStatusResult" err
                    )

                Ok data ->
                    data
                        |> unwrap
                            ( model, Cmd.none )
                            (\txReceipt ->
                                model.trackedTxs
                                    |> Dict.get (Eth.Utils.txHashToString txReceipt.hash)
                                    |> unwrap ( model, logString "TrackedTxStatusResult" "Transaction not found." )
                                        (\tx ->
                                            let
                                                ( newStatus, _, maybeUserNotice ) =
                                                    handleTxReceipt tx.chain txReceipt

                                                isMined =
                                                    case newStatus of
                                                        Mined _ ->
                                                            True

                                                        _ ->
                                                            False

                                                fetchAccounting =
                                                    (case tx.txInfo of
                                                        -- Rely on PostLogReceived as source of truth for post data.
                                                        PostTx _ ->
                                                            --maybePublishedPost
                                                            --|> Maybe.map
                                                            --(\r ->
                                                            --case r of
                                                            --LogReply p ->
                                                            --p.core
                                                            --LogRoot p ->
                                                            --p.core
                                                            --)
                                                            Nothing

                                                        TipTx id ->
                                                            Misc.getPostOrReply id model.rootPosts model.replyPosts
                                                                |> Maybe.map Misc.getCore

                                                        BurnTx id ->
                                                            Misc.getPostOrReply id model.rootPosts model.replyPosts
                                                                |> Maybe.map Misc.getCore
                                                    )
                                                        |> unwrap Cmd.none
                                                            (fetchPostInfo model.blockTimes model.config)

                                                fbEvent =
                                                    case tx.txInfo of
                                                        PostTx _ ->
                                                            Tracking.postTxMined

                                                        TipTx _ ->
                                                            Cmd.none

                                                        BurnTx _ ->
                                                            Cmd.none

                                                ( newGtagHistory, maybeGtagCmd ) =
                                                    if isMined then
                                                        let
                                                            ( actionName, label ) =
                                                                -- question:
                                                                -- is there a way to get postId here for a post?
                                                                case tx.txInfo of
                                                                    PostTx txHash ->
                                                                        ( "post tx mined"
                                                                        , Eth.Utils.txHashToString txHash
                                                                        )

                                                                    TipTx postId ->
                                                                        ( "tip tx mined"
                                                                        , Post.postIdToString postId
                                                                        )

                                                                    BurnTx postId ->
                                                                        ( "burn tx mined"
                                                                        , Post.postIdToString postId
                                                                        )
                                                        in
                                                        GTag.gTagOutOnlyOnLabelOrValueChange model.gtagHistory <|
                                                            GTagData
                                                                actionName
                                                                Nothing
                                                                (Just label)
                                                                Nothing

                                                    else
                                                        ( model.gtagHistory
                                                        , Cmd.none
                                                        )
                                            in
                                            ( { model
                                                | trackedTxs =
                                                    model.trackedTxs
                                                        |> Dict.update
                                                            (Eth.Utils.txHashToString txReceipt.hash)
                                                            (Maybe.map
                                                                (\info ->
                                                                    if info.status == Mining then
                                                                        { info | status = newStatus }

                                                                    else
                                                                        info
                                                                )
                                                            )
                                                , userNotices =
                                                    model.userNotices
                                                        ++ (maybeUserNotice
                                                                |> unwrap [] List.singleton
                                                           )
                                                , gtagHistory = newGtagHistory
                                              }
                                            , Cmd.batch
                                                [ if isMined then
                                                    fetchAccounting

                                                  else
                                                    Cmd.none
                                                , maybeGtagCmd
                                                , fbEvent
                                                ]
                                            )
                                        )
                            )

        WalletResponse res ->
            res
                |> unpack
                    (\err ->
                        case err of
                            WalletDisconnected ->
                                ( { model
                                    | wallet = NetworkReady
                                  }
                                , Cmd.none
                                )

                            WalletInProgress ->
                                ( { model
                                    | userNotices = UN.unexpectedError "Please complete the wallet connection process." :: model.userNotices
                                  }
                                , Cmd.none
                                )

                            WalletCancel ->
                                ( { model
                                    | userNotices = UN.unexpectedError "The wallet connection has been cancelled." :: model.userNotices
                                    , wallet = NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            NetworkNotSupported ->
                                ( { model
                                    | userNotices = UN.unexpectedError "This network is not supported by SmokeSignal." :: model.userNotices
                                    , wallet = NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            WalletError e ->
                                ( { model
                                    | wallet =
                                        Types.NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , logString "WalletResponse" e
                                )
                    )
                    (\info ->
                        let
                            ( gtagHistory, walletConnectedGtagCmd ) =
                                gTagOutOnlyOnceForEvent model.gtagHistory <|
                                    GTagData
                                        "wallet connected"
                                        Nothing
                                        (Just <| Eth.Utils.addressToString info.address)
                                        Nothing

                            fbEvent =
                                if model.wallet == Connecting then
                                    Tracking.metaMaskConnected

                                else
                                    Cmd.none
                        in
                        ( { model
                            | wallet = Active info
                            , chainSwitchInProgress = False
                            , gtagHistory = gtagHistory
                            , compose =
                                model.compose
                                    |> (\r ->
                                            { r
                                                | message = Nothing
                                                , error = Nothing
                                                , modal = False
                                            }
                                       )
                          }
                        , [ walletConnectedGtagCmd
                          , fbEvent
                          ]
                            |> Cmd.batch
                        )
                    )

        BalanceResponse val ->
            val
                |> unwrap
                    ( model
                    , logString "BalanceResponse" "Missing balance"
                    )
                    (\balance ->
                        ensureUserInfo
                            (\userInfo ->
                                ( { model
                                    | wallet =
                                        Active
                                            { userInfo
                                                | balance = balance
                                            }
                                  }
                                , Cmd.none
                                )
                            )
                    )

        EventSentryMsg chain eventMsg ->
            case chain of
                Eth ->
                    let
                        ( newEventSentry, cmd ) =
                            model.sentries.ethereum
                                |> unwrap ( Nothing, Cmd.none )
                                    (Sentry.update
                                        eventMsg
                                    )
                    in
                    ( { model
                        | sentries =
                            model.sentries
                                |> (\ss ->
                                        { ss
                                            | ethereum =
                                                newEventSentry
                                        }
                                   )
                      }
                    , cmd
                    )

                XDai ->
                    let
                        ( newEventSentry, cmd ) =
                            model.sentries.xDai
                                |> unwrap ( Nothing, Cmd.none )
                                    (Sentry.update
                                        eventMsg
                                    )
                    in
                    ( { model
                        | sentries =
                            model.sentries
                                |> (\ss ->
                                        { ss
                                            | xDai =
                                                newEventSentry
                                        }
                                   )
                      }
                    , cmd
                    )

        PostLogReceived res ->
            case res.returnData of
                Err err ->
                    ( model
                    , err
                        |> Json.Decode.errorToString
                        |> String.left 200
                        |> logString "PostLogReceived"
                    )

                Ok log ->
                    --|> updateTrackedTxByTxHash
                    --log.transactionHash
                    --(\trackedTx ->
                    --{ trackedTx
                    --| status =
                    --Mined <|
                    --Just <|
                    --Post.Id
                    --log.blockNumber
                    --ssPost.hash
                    --}
                    --)
                    let
                        core =
                            Misc.getCore log
                    in
                    ( addPost log model
                    , fetchPostInfo model.blockTimes model.config core
                    )

        PostAccountingFetched postId res ->
            case res of
                Ok accountingData ->
                    let
                        key =
                            Misc.postIdToKey postId

                        maybeTopic =
                            model.rootPosts
                                |> Dict.get key
                                |> Maybe.map .topic

                        accounting =
                            model.accounting
                                |> Dict.insert key accountingData

                        updateTopics =
                            maybeTopic
                                |> unwrap identity
                                    (\topic ->
                                        Dict.update
                                            topic
                                            (unwrap
                                                { total = accountingData.totalBurned
                                                , ids = Set.singleton key
                                                }
                                                (\data ->
                                                    let
                                                        ids =
                                                            data.ids
                                                                |> Set.insert key

                                                        total =
                                                            if Set.size data.ids == Set.size ids then
                                                                data.total

                                                            else
                                                                ids
                                                                    |> Set.toList
                                                                    |> List.filterMap
                                                                        (\id ->
                                                                            Dict.get id accounting
                                                                        )
                                                                    |> List.map .totalBurned
                                                                    |> List.foldl
                                                                        TokenValue.add
                                                                        TokenValue.zero
                                                    in
                                                    { total = total
                                                    , ids = ids
                                                    }
                                                )
                                                >> Just
                                            )
                                    )
                    in
                    ( { model
                        | accounting = accounting
                        , topics =
                            model.topics
                                |> updateTopics
                        , pages =
                            model.rootPosts
                                |> calculatePagination
                                    model.sortType
                                    model.blockTimes
                                    accounting
                                    model.now
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , logHttpError "PostAccountingFetched" err
                    )

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err err ->
                    ( model
                    , logHttpError "BlockTimeFetched" err
                    )

                Ok time ->
                    ( { model
                        | blockTimes =
                            model.blockTimes
                                |> Dict.insert blocknum time
                      }
                    , Cmd.none
                    )

        DismissNotice id ->
            ( { model
                | userNotices =
                    model.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        GotoView view ->
            ( model
            , view
                |> Routing.viewUrlToPathString
                |> Ports.pushUrl
            )

        ConnectToWeb3 ->
            if model.wallet == NoneDetected then
                ( model, Cmd.none )

            else
                let
                    gtagCmd =
                        GTagData
                            "wallet connect initiated"
                            Nothing
                            Nothing
                            Nothing
                            |> gTagOut
                in
                ( { model
                    | wallet = Connecting
                  }
                , [ Ports.connectToWeb3 ()
                  , gtagCmd
                  ]
                    |> Cmd.batch
                )

        ShowOrHideAddress phaceId ->
            ( { model
                | showAddressId =
                    if model.showAddressId == Just phaceId then
                        Nothing

                    else
                        Just phaceId
              }
            , Cmd.none
            )

        PriceResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\_ ->
                                let
                                    compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | inProgress = False
                                                        , error = Just "There has been a problem."
                                                    }
                                               )
                                in
                                ( { model | compose = compose }, Cmd.none )
                            )
                            (\price ->
                                model.compose.dollar
                                    |> getPostBurnAmount price
                                    |> Result.andThen
                                        (\burnAmount ->
                                            let
                                                donateAmount =
                                                    if TokenValue.isZero burnAmount then
                                                        TokenValue.zero

                                                    else
                                                        TokenValue.divByInt 100 burnAmount

                                                lowBalance =
                                                    --TokenValue.compare
                                                    --(TokenValue.add burnAmount donateAmount)
                                                    --userInfo.balance
                                                    --/= LT
                                                    False

                                                context =
                                                    case model.compose.context of
                                                        Types.TopLevel topic ->
                                                            model.topicInput
                                                                |> Misc.validateTopic
                                                                |> Maybe.withDefault topic
                                                                |> TopLevel

                                                        ctx ->
                                                            ctx

                                                metadata =
                                                    { metadataVersion =
                                                        Post.currentMetadataVersion
                                                    , context = context
                                                    , maybeDecodeError = Nothing
                                                    }

                                                content =
                                                    { title =
                                                        if String.isEmpty model.compose.title then
                                                            Nothing

                                                        else
                                                            Just model.compose.title
                                                    , desc = Nothing
                                                    , body = model.compose.body
                                                    }
                                            in
                                            if lowBalance then
                                                Err "Not enough funds."

                                            else
                                                { donateAmount = donateAmount
                                                , author = userInfo.address
                                                , authorBurn = burnAmount
                                                , content = content
                                                , metadata = metadata
                                                }
                                                    |> Ok
                                        )
                                    |> unpack
                                        (\err ->
                                            let
                                                compose =
                                                    model.compose
                                                        |> (\r ->
                                                                { r
                                                                    | inProgress = False
                                                                    , error = Just err
                                                                }
                                                           )
                                            in
                                            ( { model
                                                | compose = compose
                                              }
                                            , GTagData
                                                "post failed"
                                                Nothing
                                                (err
                                                    |> Just
                                                )
                                                Nothing
                                                |> gTagOut
                                            )
                                        )
                                        (\postDraft ->
                                            let
                                                config =
                                                    Chain.getConfig userInfo.chain model.config

                                                txParams =
                                                    postDraft
                                                        |> SSContract.burnEncodedPost userInfo config.contract
                                                        |> Eth.toSend
                                                        |> Eth.encodeSend
                                            in
                                            ( model
                                            , [ Ports.submitPost txParams
                                              , GTagData
                                                    "post tx signing"
                                                    Nothing
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                              ]
                                                |> Cmd.batch
                                            )
                                        )
                            )
                )

        SharePost core ->
            let
                title =
                    core.content.title
                        |> Maybe.withDefault "SmokeSignal"

                url =
                    Routing.viewUrlToPathString (ViewPost core.id)
            in
            ( model
            , Misc.encodeShare title url
                |> Ports.share
            )

        SubmitDraft ->
            ensureUserInfo
                (\userInfo ->
                    let
                        compose =
                            model.compose
                                |> (\r ->
                                        { r
                                            | inProgress = True
                                            , error = Nothing
                                        }
                                   )
                    in
                    ( { model
                        | compose = compose
                      }
                    , SSContract.getEthPriceCmd
                        (Chain.getConfig userInfo.chain model.config)
                        |> Task.attempt PriceResponse
                    )
                )

        SubmitTipOrBurn ->
            ensureUserInfo
                (\userInfo ->
                    model.maybeBurnOrTipUX
                        |> unwrap ( model, Cmd.none )
                            (\burnOrTipUX ->
                                burnOrTipUX.input
                                    |> String.toFloat
                                    |> unwrap
                                        ( { model
                                            | maybeBurnOrTipUX =
                                                Just
                                                    { burnOrTipUX
                                                        | error =
                                                            Just "Invalid tip amount"
                                                    }
                                          }
                                        , Cmd.none
                                        )
                                        (\amount ->
                                            let
                                                postState =
                                                    { burnOrTipUX
                                                        | inProgress = True
                                                        , error = Nothing
                                                    }

                                                txState =
                                                    { postHash = burnOrTipUX.id.messageHash
                                                    , amount = amount
                                                    , txType = burnOrTipUX.burnOrTip
                                                    }
                                            in
                                            ( { model
                                                | maybeBurnOrTipUX = Just postState
                                              }
                                            , SSContract.getEthPriceCmd
                                                (Chain.getConfig userInfo.chain model.config)
                                                |> Task.attempt
                                                    (BurnOrTipPriceResponse txState)
                                            )
                                        )
                            )
                )

        BurnOrTipPriceResponse state res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\_ ->
                                let
                                    maybeBurnOrTipUX =
                                        model.maybeBurnOrTipUX
                                            |> Maybe.map
                                                (\r ->
                                                    { r
                                                        | inProgress = False
                                                        , error = Just "There has been a problem."
                                                    }
                                                )

                                    ( newGtagHistory, gtagCmd ) =
                                        GTagData
                                            "burn or tip error"
                                            Nothing
                                            (Misc.obscureAddress userInfo.address
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
                                in
                                ( { model
                                    | maybeBurnOrTipUX = maybeBurnOrTipUX
                                    , gtagHistory = newGtagHistory
                                  }
                                , gtagCmd
                                )
                            )
                            (\price ->
                                let
                                    amount =
                                        TokenValue.fromFloatWithWarning (state.amount / price)

                                    config =
                                        Chain.getConfig userInfo.chain model.config

                                    ( fn, tipOrBurn ) =
                                        case state.txType of
                                            Tip ->
                                                ( SSContract.tipForPost, "tip" )

                                            Burn ->
                                                ( SSContract.burnForPost, "burn" )

                                    txParams =
                                        fn userInfo config.contract state.postHash amount TokenValue.zero
                                            |> Eth.toSend
                                            |> Eth.encodeSend

                                    gtagCmd =
                                        GTagData
                                            tipOrBurn
                                            Nothing
                                            ((state.postHash
                                                |> Eth.Utils.hexToString
                                             )
                                                ++ tipOrBurn
                                                ++ "ed"
                                                |> Just
                                            )
                                            Nothing
                                            |> gTagOut
                                in
                                ( model
                                , [ Ports.submitBurnOrTip txParams
                                  , gtagCmd
                                  ]
                                    |> Cmd.batch
                                )
                            )
                )

        SetPage n ->
            let
                gtagCmd =
                    GTagData
                        "set page"
                        Nothing
                        Nothing
                        (Just n)
                        |> gTagOut
            in
            ( { model
                | currentPage = n
              }
            , gtagCmd
            )

        StartBurnOrTipUX id burnOrTip ->
            -- discuss
            ( { model
                | maybeBurnOrTipUX =
                    { id = id
                    , input = ""
                    , burnOrTip = burnOrTip
                    , inProgress = False
                    , error = Nothing
                    }
                        |> Just
              }
            , Cmd.none
            )

        CancelPostInput ->
            let
                gtagCmd =
                    GTagData
                        "compose post cancel"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | maybeBurnOrTipUX = Nothing
              }
            , gtagCmd
            )

        ChangeDemoPhaceSrc ->
            ( model
            , Random.generate NewDemoSrc DemoPhaceSrcMutator.addressSrcGenerator
            )

        NewDemoSrc src ->
            ( { model | demoPhaceSrc = src }
            , Cmd.none
            )

        XDaiImport ->
            let
                label =
                    case userInfo model.wallet of
                        Nothing ->
                            "not connected"

                        Just userInfo ->
                            Misc.obscureAddress userInfo.address

                gtagCmd =
                    GTagData
                        "xdai import clicked"
                        Nothing
                        (Just label)
                        Nothing
                        |> gTagOut
            in
            ( { model | chainSwitchInProgress = True }
            , [ Ports.xDaiImport ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        CookieConsentGranted ->
            let
                gtagCmd =
                    GTagData
                        "accept cookies"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | cookieConsentGranted = True
              }
            , [ Ports.consentToCookies ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        ShowNewToSmokeSignalModal flag ->
            let
                ( newGtagHistory, gtagCmd ) =
                    GTagData
                        "show new to smokesignal"
                        Nothing
                        ((if flag == True then
                            "True"

                          else
                            "False"
                         )
                            |> Just
                        )
                        Nothing
                        |> gTagOutOnlyOnLabelOrValueChange model.gtagHistory
            in
            ( { model
                | newUserModal = flag
                , gtagHistory = newGtagHistory
              }
            , [ Ports.setVisited ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        SubmitFaucet ->
            ensureUserInfo
                (\userInfo ->
                    let
                        addr =
                            userInfo.address
                                |> Eth.Utils.addressToString
                    in
                    ( { model
                        | compose =
                            model.compose
                                |> (\r ->
                                        { r
                                            | message = Nothing
                                            , error = Nothing
                                        }
                                   )
                        , wallet =
                            Active
                                { userInfo
                                    | faucetStatus =
                                        FaucetStatus RequestInProgress
                                }
                      }
                    , [ Http.get
                            { url = "https://personal-rxyx.outsystemscloud.com/ERC20FaucetRest/rest/v1/send?In_ReceiverErc20Address=" ++ addr ++ "&In_Token=" ++ model.faucetToken
                            , expect =
                                Http.expectJson
                                    FaucetResponse
                                    Misc.decodeFaucetResponse
                            }
                      , Tracking.faucetRequestInitiated
                      ]
                        |> Cmd.batch
                    )
                )

        FaucetResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\e ->
                                ( { model
                                    | compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | message =
                                                            Just "There has been a problem."
                                                    }
                                               )
                                    , wallet =
                                        Active
                                            { userInfo
                                                | faucetStatus =
                                                    FaucetStatus (RequestError "There has been a problem.")
                                            }
                                  }
                                , logHttpError "FaucetResponse" e
                                )
                            )
                            (\data ->
                                let
                                    faucetSuccess =
                                        data.status
                                in
                                ( { model
                                    | wallet =
                                        Active
                                            { userInfo
                                                | faucetStatus =
                                                    if faucetSuccess then
                                                        FaucetSuccess

                                                    else
                                                        FaucetStatus (RequestError data.message)
                                            }
                                    , compose =
                                        model.compose
                                            |> (\r ->
                                                    { r
                                                        | message =
                                                            if faucetSuccess then
                                                                Just "Your faucet request was successful. Check your wallet for updated balance."

                                                            else
                                                                Just data.message
                                                    }
                                               )
                                  }
                                , if faucetSuccess then
                                    Cmd.batch
                                        [ Tracking.xDaiClaimCompleted
                                        , userInfo.address
                                            |> Eth.Utils.addressToString
                                            |> Ports.refreshWallet
                                        ]

                                  else
                                    Cmd.none
                                )
                            )
                )

        TopicSubmit ->
            (if String.isEmpty model.topicInput then
                Misc.defaultTopic

             else
                model.topicInput
            )
                |> Misc.validateTopic
                |> unwrap
                    ( { model
                        | userNotices =
                            [ UN.unexpectedError "Invalid topic" ]
                      }
                    , GTagData
                        "search topic invalid"
                        Nothing
                        (model.topicInput
                            |> Just
                        )
                        Nothing
                        |> gTagOut
                    )
                    (\topic ->
                        ( model
                        , [ ViewTopic topic
                                |> Routing.viewUrlToPathString
                                |> Ports.pushUrl
                          , GTagData
                                "search topic valid"
                                Nothing
                                (topic
                                    |> Just
                                )
                                Nothing
                                |> gTagOut
                          ]
                            |> Cmd.batch
                        )
                    )

        ReplyOpen id ->
            if model.wallet == NoneDetected || model.wallet == NetworkReady then
                ( { model
                    | compose =
                        { emptyComposeModel | modal = True }
                  }
                , gTagOut <|
                    GTagData
                        "onboarding initiated"
                        Nothing
                        Nothing
                        Nothing
                )

            else
                let
                    context =
                        Types.Reply id

                    gtagCmd =
                        if Wallet.isActive model.wallet then
                            GTagData
                                "reply opened"
                                Nothing
                                Nothing
                                Nothing
                                |> gTagOut

                        else
                            Cmd.none
                in
                ( { model
                    | compose =
                        { emptyComposeModel
                            | reply = True
                            , context = context
                            , title = model.compose.title
                            , body = model.compose.body
                        }
                  }
                , Cmd.batch
                    [ gtagCmd
                    , model.wallet
                        |> Wallet.userInfo
                        |> unwrap Cmd.none
                            (.address
                                >> Eth.Utils.addressToString
                                >> Ports.refreshWallet
                            )
                    ]
                )

        CloseComposeError ->
            ( { model
                | compose =
                    model.compose
                        |> (\r ->
                                { r
                                    | error = Nothing
                                }
                           )
              }
            , Cmd.none
            )

        ComposeOpen ->
            if model.wallet == NoneDetected then
                ( { model
                    | compose =
                        if Wallet.isActive model.wallet then
                            { emptyComposeModel | reply = True }

                        else
                            { emptyComposeModel | modal = True }
                  }
                , gTagOut <|
                    GTagData
                        "onboarding initiated"
                        Nothing
                        Nothing
                        Nothing
                )

            else
                let
                    topic =
                        case model.view of
                            ViewTopic t ->
                                t

                            _ ->
                                model.topicInput
                                    |> Misc.validateTopic
                                    |> Maybe.withDefault Misc.defaultTopic

                    trackingCmd =
                        if Wallet.isActive model.wallet then
                            Tracking.composePostOpened

                        else
                            Cmd.none
                in
                ( { model
                    | compose =
                        { emptyComposeModel
                            | modal = True
                            , context = Types.TopLevel topic
                            , title = model.compose.title
                            , body = model.compose.body
                        }
                    , topicInput = topic
                  }
                , Cmd.batch
                    [ trackingCmd
                    , model.wallet
                        |> Wallet.userInfo
                        |> unwrap Cmd.none
                            (.address
                                >> Eth.Utils.addressToString
                                >> Ports.refreshWallet
                            )
                    ]
                )

        ComposeClose ->
            let
                gtagCmd =
                    GTagData
                        "compose post closed"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | compose =
                    model.compose
                        |> (\r ->
                                { r
                                    | modal = False
                                    , message = Nothing
                                    , error = Nothing
                                    , reply = False
                                }
                           )
              }
            , gtagCmd
            )

        ComposeBodyChange str ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | body = str })
              }
            , Cmd.none
            )

        ComposeTitleChange str ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | title = str })
              }
            , Cmd.none
            )

        ComposeDollarChange str ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | dollar = str })
              }
            , Cmd.none
            )

        TopicInputChange str ->
            ( { model
                | topicInput = str
              }
            , Cmd.none
            )

        BurnOrTipUXInputChange str ->
            ( { model
                | maybeBurnOrTipUX =
                    model.maybeBurnOrTipUX
                        |> Maybe.map (\r -> { r | input = str })
              }
            , Cmd.none
            )

        SanitizeTopic ->
            ( { model
                | topicInput =
                    model.topicInput
                        |> Misc.validateTopic
                        |> Maybe.withDefault Misc.defaultTopic
              }
            , Cmd.none
            )

        SetSortType newSortType ->
            let
                gtagCmd =
                    GTagData
                        ("change sort type: " ++ sortTypeToString newSortType)
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | sortType = newSortType
                , pages =
                    calculatePagination
                        newSortType
                        model.blockTimes
                        model.accounting
                        model.now
                        model.rootPosts
              }
            , gtagCmd
            )

        ToggleTooltip tooltipId ->
            ( { model
                | maybeActiveTooltip =
                    if model.maybeActiveTooltip == Just tooltipId then
                        Nothing

                    else
                        Just tooltipId
              }
            , Cmd.none
            )

        ScrollResponse _ ->
            ( model, Cmd.none )


handleRoute : Model -> Route -> ( Model, Cmd Msg )
handleRoute model route =
    let
        defaultTitle =
            Ports.setTitle "SmokeSignal | Uncensorable - Immutable - Unkillable | Real Free Speech - Cemented on the Blockchain"

        resetScroll =
            Browser.Dom.setViewportOf Misc.scrollId 0 0
                |> Task.attempt ScrollResponse
    in
    (case route of
        RouteTopics ->
            ( { model
                | view = ViewTopics
              }
            , [ defaultTitle ]
            )

        RouteCompose ->
            if model.wallet == NoneDetected then
                ( { model
                    | compose =
                        if Wallet.isActive model.wallet then
                            { emptyComposeModel | reply = True }

                        else
                            { emptyComposeModel | modal = True }
                    , view = ViewCompose
                  }
                , [ GTagData
                        "onboarding initiated"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
                  , defaultTitle
                  ]
                )

            else
                let
                    topic =
                        case model.view of
                            ViewTopic t ->
                                t

                            _ ->
                                model.topicInput
                                    |> Misc.validateTopic
                                    |> Maybe.withDefault Misc.defaultTopic

                    trackingCmd =
                        if Wallet.isActive model.wallet then
                            Tracking.composePostOpened

                        else
                            Cmd.none
                in
                ( { model
                    | compose =
                        { emptyComposeModel
                            | context = Types.TopLevel topic
                            , title = model.compose.title
                            , body = model.compose.body
                        }
                    , topicInput = topic
                    , view = ViewCompose
                  }
                , [ trackingCmd
                  , model.wallet
                        |> Wallet.userInfo
                        |> unwrap Cmd.none
                            (.address
                                >> Eth.Utils.addressToString
                                >> Ports.refreshWallet
                            )
                  , defaultTitle
                  ]
                )

        RouteHome ->
            ( { model
                | view = ViewHome
              }
            , [ defaultTitle ]
            )

        RouteTxns ->
            ( { model
                | view = ViewTxns
              }
            , [ defaultTitle ]
            )

        RouteWallet ->
            ( { model
                | view = ViewWallet
              }
            , [ defaultTitle ]
            )

        RouteAbout ->
            ( { model
                | view = ViewAbout
              }
            , [ defaultTitle ]
            )

        RouteUser addr ->
            ( { model
                | view = ViewUser addr
              }
            , [ defaultTitle ]
            )

        RouteInvalid ->
            ( { model
                | userNotices =
                    [ UN.routeNotFound Nothing ]
              }
            , [ defaultTitle ]
            )

        RouteViewPost id ->
            ( { model
                | view = ViewPost id
              }
            , [ Dict.get (postIdToKey id) model.rootPosts
                    |> unwrap defaultTitle
                        (\post ->
                            [ post.core.content.title
                                |> unwrap defaultTitle
                                    (\title ->
                                        title
                                            ++ " | SmokeSignal"
                                            |> Ports.setTitle
                                    )
                            , post.core.content.desc
                                |> unwrap Cmd.none Ports.setDescription
                            ]
                                |> Cmd.batch
                        )
              , Tracking.viewPost id
              ]
            )

        RouteTopic topic ->
            topic
                |> Misc.validateTopic
                |> unwrap
                    ( { model
                        | userNotices =
                            [ UN.routeNotFound Nothing ]
                        , view = ViewHome
                      }
                    , [ defaultTitle ]
                    )
                    (\t ->
                        ( { model
                            | view = ViewTopic t
                          }
                        , [ "Discussions related to #"
                                ++ topic
                                ++ " on SmokeSignal"
                                |> Ports.setDescription
                          , "#"
                                ++ topic
                                ++ " | SmokeSignal"
                                |> Ports.setTitle
                          ]
                        )
                    )
    )
        |> Tuple.mapSecond
            (\cmds ->
                (resetScroll :: cmds) |> Cmd.batch
            )


addPost : LogPost -> Model -> Model
addPost log model =
    case log of
        LogRoot post ->
            let
                rootPosts =
                    model.rootPosts
                        |> Dict.insert post.core.key post
            in
            { model
                | rootPosts = rootPosts
                , topics =
                    model.topics
                        |> Dict.update
                            post.topic
                            (Maybe.withDefault
                                { total = TokenValue.zero
                                , ids = Set.empty
                                }
                                >> Just
                            )
                , pages =
                    rootPosts
                        |> calculatePagination
                            model.sortType
                            model.blockTimes
                            model.accounting
                            model.now
            }

        LogReply post ->
            { model
                | replyPosts =
                    model.replyPosts
                        |> Dict.insert post.core.key post
                , replyIds =
                    model.replyIds
                        |> Dict.update (Misc.postIdToKey post.parent)
                            (Maybe.withDefault Set.empty
                                >> Set.insert post.core.key
                                >> Just
                            )
            }


handleTxReceipt : Chain -> TxReceipt -> ( TxStatus, Maybe LogPost, Maybe UserNotice )
handleTxReceipt chain txReceipt =
    case txReceipt.status of
        Just True ->
            let
                post =
                    txReceipt.logs
                        |> List.map
                            (SSContract.decodePost chain
                                >> .returnData
                            )
                        |> List.filterMap Result.toMaybe
                        |> List.head
            in
            ( post
                |> Maybe.map
                    (\p ->
                        case p of
                            LogReply x ->
                                x.core.id

                            LogRoot x ->
                                x.core.id
                    )
                |> Mined
            , post
            , Nothing
            )

        Just False ->
            ( Failed Types.MinedButExecutionFailed
            , Nothing
            , Nothing
            )

        Nothing ->
            ( Mining
            , Nothing
            , Just <|
                UN.unexpectedError "Weird. I Got a transaction receipt with a success value of 'Nothing'. Depending on why this happened I might be a little confused about any mining transactions."
            )


fetchPostInfo : Dict Int Time.Posix -> Config -> Core -> Cmd Msg
fetchPostInfo blockTimes config core =
    [ SSContract.getAccountingCmd
        (Chain.getConfig core.chain config)
        core.id.messageHash
        |> Task.attempt (PostAccountingFetched core.id)
    , if Dict.member core.id.block blockTimes then
        Cmd.none

      else
        Eth.getBlock
            (Chain.getProviderUrl core.chain config)
            core.id.block
            |> Task.map .timestamp
            |> Task.attempt (BlockTimeFetched core.id.block)
    ]
        |> Cmd.batch


logHttpError : String -> Http.Error -> Cmd msg
logHttpError tag =
    Misc.parseHttpError >> (++) (tag ++ ":\n") >> Ports.log


logString : String -> String -> Cmd msg
logString tag =
    (++) (tag ++ ":\n") >> Ports.log


getPostBurnAmount : Float -> String -> Result String TokenValue
getPostBurnAmount price txt =
    if String.isEmpty txt then
        Ok TokenValue.zero

    else
        txt
            |> Misc.dollarStringToToken price
            |> Result.fromMaybe "Invalid burn amount"


calculatePagination : SortType -> Dict Int Time.Posix -> Dict PostKey Accounting -> Time.Posix -> Dict PostKey RootPost -> Array.Array (List PostKey)
calculatePagination sortType blockTimes accounting now =
    Dict.values
        >> List.sortBy
            (.core
                >> Misc.sortPostsFunc sortType blockTimes accounting now
            )
        >> List.map (.core >> .key)
        >> List.Extra.greedyGroupsOf 10
        >> Array.fromList
