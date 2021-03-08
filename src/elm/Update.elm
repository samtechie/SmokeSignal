module Update exposing (update)

import Array
import Browser
import Browser.Navigation
import Contracts.SmokeSignal as SSContract
import DemoPhaceSrcMutator
import Dict exposing (Dict)
import Eth
import Eth.Sentry.Event as EventSentry
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (TxReceipt)
import Eth.Utils
import GTag exposing (GTagData, gTagOut)
import Helpers.Element as EH exposing (DisplayProfile(..))
import Http
import Json.Decode
import List.Extra
import Maybe.Extra exposing (unwrap)
import Misc
import Ports
import Post
import Random
import Result.Extra exposing (unpack)
import Routing exposing (viewToUrlString)
import Set
import Task
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import Url
import UserNotice as UN exposing (UserNotice)
import Wallet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ensureUserInfo fn =
            model.wallet
                |> Wallet.userInfo
                |> unwrap ( model, Ports.log "Missing wallet" ) fn
    in
    case msg of
        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            Browser.Navigation.pushUrl model.navKey (Url.toString url)

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( model, cmd )

        RouteChanged route ->
            handleRoute { model | hasNavigated = True } route

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
                    EH.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        EveryFewSeconds ->
            ( model
            , SSContract.getEthPriceCmd
                model.config.ethereum
                |> Task.attempt EthPriceFetched
            )

        ShowExpandedTrackedTxs flag ->
            ( { model
                | showExpandedTrackedTxs = flag
              }
            , Cmd.none
            )

        RpcResponse res ->
            res
                |> unpack
                    (\e ->
                        ( model, logHttpError "RpcResponse" e )
                    )
                    (\info ->
                        ( { model
                            | wallet = Active info
                            , postState = Nothing
                          }
                        , Cmd.none
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
                            (Misc.getProviderUrl tx.chain model.config)
                            tx.txHash
                            |> Task.attempt TrackedTxStatusResult
                    )
                |> Cmd.batch
            )

        TrackedTxStatusResult res ->
            case res of
                Err err ->
                    ( model, logHttpError "TrackedTxStatusResult" err )

                Ok data ->
                    data
                        |> unwrap
                            ( model, Cmd.none )
                            (\txReceipt ->
                                model.trackedTxs
                                    |> Dict.get (Eth.Utils.txHashToString txReceipt.hash)
                                    |> unwrap ( model, Ports.log "Transaction not found." )
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

                                                        TipTx id _ ->
                                                            Misc.getPostOrReply id model

                                                        BurnTx id _ ->
                                                            Misc.getPostOrReply id model
                                                    )
                                                        |> unwrap Cmd.none
                                                            (fetchPostInfo model.blockTimes model.config)
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
                                              }
                                            , if isMined then
                                                fetchAccounting

                                              else
                                                Cmd.none
                                            )
                                        )
                            )

        WalletResponse res ->
            case res of
                WalletSucceed info ->
                    ( { model
                        | wallet = Active info
                      }
                    , Cmd.none
                    )

                WalletInProgress ->
                    ( { model
                        | userNotices = UN.unexpectedError "Please complete the wallet connection process" :: model.userNotices
                      }
                    , Cmd.none
                    )

                WalletCancel ->
                    ( { model
                        | userNotices = UN.unexpectedError "The wallet connection has been cancelled" :: model.userNotices
                        , wallet = NetworkReady
                      }
                    , Cmd.none
                    )

                WalletError ->
                    ( { model
                        | wallet =
                            Types.NetworkReady
                      }
                    , Cmd.none
                    )

        TxSentryMsg chain subMsg ->
            case chain of
                Eth ->
                    let
                        ( newTxSentry, subCmd ) =
                            TxSentry.update subMsg model.txSentry
                    in
                    ( { model | txSentry = newTxSentry }, subCmd )

                XDai ->
                    let
                        ( newTxSentry, subCmd ) =
                            TxSentry.update subMsg model.txSentryX
                    in
                    ( { model | txSentryX = newTxSentry }, subCmd )

        EventSentryMsg chain eventMsg ->
            case chain of
                Eth ->
                    let
                        ( newEventSentry, cmd ) =
                            EventSentry.update
                                eventMsg
                                model.sentries.ethereum
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
                            EventSentry.update
                                eventMsg
                                model.sentries.xDai
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
                        |> (++) "PostLogReceived:\n"
                        |> Ports.log
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
                            case log of
                                LogReply p ->
                                    p.core

                                LogRoot p ->
                                    p.core
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
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                        |> addUserNotice (UN.web3FetchError "accounting data")
                    , logHttpError "PostAccountingFetched" err
                    )

        BalanceFetched address res ->
            case res of
                Ok balance ->
                    ( { model
                        | wallet =
                            model.wallet
                                |> Wallet.userInfo
                                |> unwrap
                                    model.wallet
                                    (\userInfo ->
                                        Active
                                            { userInfo
                                                | balance =
                                                    if userInfo.address == address then
                                                        balance

                                                    else
                                                        userInfo.balance
                                            }
                                    )
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                    , logHttpError "BalanceFetched" err
                    )

        EthPriceFetched fetchResult ->
            case fetchResult of
                Ok price ->
                    ( { model
                        | ethPrice = price
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                        |> addUserNotice (UN.web3FetchError "ETH price")
                    , logHttpError "EthPriceFetched" err
                    )

        XDaiPriceFetched fetchResult ->
            case fetchResult of
                Ok price ->
                    ( { model
                        | xDaiPrice = price
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model
                        |> addUserNotice (UN.web3FetchError "xDai price")
                    , logHttpError "XDaiPriceFetched" err
                    )

        BlockTimeFetched blocknum timeResult ->
            case timeResult of
                Err err ->
                    ( model
                        |> addUserNotice (UN.web3FetchError "block time")
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

        TxSigned chain txInfo res ->
            case res of
                Ok txHash ->
                    ( { model
                        | showExpandedTrackedTxs = True
                        , trackedTxs =
                            model.trackedTxs
                                |> Dict.insert (Eth.Utils.txHashToString txHash)
                                    { txHash = txHash
                                    , txInfo = txInfo
                                    , status = Mining
                                    , chain = chain
                                    }
                      }
                    , Cmd.none
                    )

                Err errStr ->
                    ( model
                    , Ports.log <| "TxSigned error:\n" ++ errStr
                    )

        GotoView view ->
            ( model
            , Browser.Navigation.pushUrl
                model.navKey
                (Routing.viewToUrlString view)
            )

        ConnectToWeb3 ->
            ( { model | wallet = Connecting }
            , Ports.connectToWeb3 ()
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

        AddUserNotice userNotice ->
            ( model |> addUserNotice userNotice
            , Cmd.none
            )

        SubmitDraft ->
            ensureUserInfo
                (\userInfo ->
                    model.compose.dollar
                        |> getPostBurnAmount
                            (Misc.getPrice userInfo.chain model)
                        |> Result.andThen
                            (\burnAmount ->
                                let
                                    donateAmount =
                                        if model.compose.donate then
                                            TokenValue.div burnAmount 100

                                        else
                                            TokenValue.zero

                                    lowBalance =
                                        TokenValue.compare
                                            (TokenValue.add burnAmount donateAmount)
                                            userInfo.balance
                                            /= LT

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
                                ( { model
                                    | userNotices = [ UN.unexpectedError err ]
                                  }
                                , Cmd.none
                                )
                            )
                            (\postDraft ->
                                let
                                    config =
                                        Misc.getConfig userInfo.chain model.config

                                    txParams =
                                        postDraft
                                            |> SSContract.burnEncodedPost userInfo config.contract
                                            |> Eth.toSend

                                    listeners =
                                        { onMined = Nothing
                                        , onSign = Just <| TxSigned userInfo.chain <| PostTx postDraft
                                        , onBroadcast = Nothing
                                        }
                                in
                                submitTxn model userInfo.chain listeners txParams
                            )
                )

        SubmitBurn txt postId ->
            ensureUserInfo
                (\userInfo ->
                    txt
                        |> Misc.dollarStringToToken
                            (Misc.getPrice userInfo.chain model)
                        |> Result.fromMaybe "Invalid burn amount"
                        |> unpack
                            (\err ->
                                ( { model
                                    | userNotices = [ UN.unexpectedError err ]
                                  }
                                , Cmd.none
                                )
                            )
                            (\amount ->
                                let
                                    config =
                                        Misc.getConfig userInfo.chain model.config

                                    txParams =
                                        SSContract.burnForPost userInfo config.contract postId.messageHash amount model.compose.donate
                                            |> Eth.toSend

                                    listeners =
                                        { onMined = Nothing
                                        , onSign = Just <| TxSigned userInfo.chain <| BurnTx postId amount
                                        , onBroadcast = Nothing
                                        }
                                in
                                submitTxn model userInfo.chain listeners txParams
                            )
                )

        SubmitTip txt postId ->
            ensureUserInfo
                (\userInfo ->
                    txt
                        |> Misc.dollarStringToToken
                            (Misc.getPrice userInfo.chain model)
                        |> Result.fromMaybe "Invalid tip amount"
                        |> unpack
                            (\err ->
                                ( { model
                                    | userNotices = [ UN.unexpectedError err ]
                                  }
                                , Cmd.none
                                )
                            )
                            (\amount ->
                                let
                                    config =
                                        Misc.getConfig userInfo.chain model.config

                                    txParams =
                                        SSContract.tipForPost userInfo config.contract postId.messageHash amount model.compose.donate
                                            |> Eth.toSend

                                    listeners =
                                        { onMined = Nothing
                                        , onSign = Just <| TxSigned userInfo.chain <| TipTx postId amount
                                        , onBroadcast = Nothing
                                        }
                                in
                                submitTxn model userInfo.chain listeners txParams
                            )
                )

        DonationCheckboxSet flag ->
            ( { model
                | compose =
                    model.compose
                        |> (\r -> { r | donate = flag })
              }
            , Cmd.none
            )

        ViewDraft maybeDraft ->
            ( { model
                | draftModal = maybeDraft
              }
            , Cmd.none
            )

        SetPage n ->
            ( { model
                | currentPage = n
              }
            , Cmd.none
            )

        SetTipOpen state ->
            ( { model
                | postState = Just state
              }
            , Cmd.none
            )

        CancelTipOpen ->
            ( { model
                | postState = Nothing
              }
            , Cmd.none
            )

        ChangeDemoPhaceSrc ->
            ( model
              --, Random.generate MutateDemoSrcWith mutateInfoGenerator
            , Random.generate NewDemoSrc DemoPhaceSrcMutator.addressSrcGenerator
            )

        NewDemoSrc src ->
            ( { model | demoPhaceSrc = src }
            , Cmd.none
            )

        ClickHappened ->
            ( { model
                | showAddressId = Nothing
                , showExpandedTrackedTxs = False
                , draftModal = Nothing
              }
            , Cmd.none
            )

        XDaiImport ->
            ( model
            , Ports.xDaiImport ()
            )

        CookieConsentGranted ->
            ( { model
                | cookieConsentGranted = True
              }
            , Cmd.batch
                [ Ports.consentToCookies ()
                , gTagOut <|
                    GTagData
                        "accept cookies"
                        Nothing
                        Nothing
                        Nothing
                ]
            )

        ShowNewToSmokeSignalModal flag ->
            ( { model
                | newUserModal = flag
              }
            , Ports.setVisited ()
            )

        TopicSubmit ->
            model.topicInput
                |> Misc.validateTopic
                |> unwrap
                    ( { model
                        | userNotices =
                            [ UN.unexpectedError "Invalid topic" ]
                      }
                    , Cmd.none
                    )
                    (\topic ->
                        ( { model | topicInput = "" }
                        , Browser.Navigation.pushUrl
                            model.navKey
                            (Routing.viewToUrlString <| ViewTopic topic)
                        )
                    )

        ComposeOpen ->
            let
                topic =
                    model.topicInput
                        |> Misc.validateTopic
                        |> Maybe.withDefault Post.defaultTopic

                context =
                    case model.view of
                        ViewTopic t ->
                            Types.TopLevel t

                        ViewPost id ->
                            Types.Reply id

                        _ ->
                            Types.TopLevel topic

                topicInput =
                    case context of
                        Types.Reply _ ->
                            model.topicInput

                        Types.TopLevel t ->
                            t
            in
            ( { model
                | compose =
                    model.compose
                        |> (\r ->
                                { r
                                    | modal = True
                                    , title = ""
                                    , body = ""
                                    , dollar = ""
                                    , context = context
                                }
                           )
                , topicInput = topicInput
              }
            , Cmd.none
            )

        ComposeClose ->
            ( { model
                | compose =
                    model.compose
                        |> (\r ->
                                { r
                                    | modal = False
                                }
                           )
              }
            , Cmd.none
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

        PostInputChange str ->
            ( { model
                | postState =
                    model.postState
                        |> Maybe.map (\r -> { r | input = str })
              }
            , Cmd.none
            )

        SanitizeTopic ->
            ( { model
                | topicInput =
                    model.topicInput
                        |> Misc.validateTopic
                        |> Maybe.withDefault Post.defaultTopic
              }
            , Cmd.none
            )

        GoBack ->
            ( model
              -- To prevent navigating to a previous website
            , if model.hasNavigated then
                Browser.Navigation.back model.navKey 1

              else
                Browser.Navigation.pushUrl
                    model.navKey
                    (Routing.viewToUrlString ViewHome)
            )


handleRoute : Model -> Route -> ( Model, Cmd Msg )
handleRoute model route =
    case route of
        RouteTopics ->
            ( { model
                | view = ViewTopics
              }
            , Cmd.none
            )

        RouteHome ->
            ( { model
                | view = ViewHome
              }
            , Cmd.none
            )

        RouteTxns ->
            ( { model
                | view = ViewTxns
              }
            , Cmd.none
            )

        RouteWallet ->
            ( { model
                | view = ViewWallet
              }
            , Cmd.none
            )

        RouteInvalid ->
            ( { model
                | userNotices =
                    [ UN.routeNotFound Nothing ]
              }
            , Cmd.none
            )

        RouteViewPost id ->
            ( { model
                | view = ViewPost id
              }
            , Dict.get (Misc.postIdToKey id) model.rootPosts
                |> Maybe.andThen (.core >> .content >> .desc)
                |> unwrap Cmd.none Ports.setDescription
            )

        RouteMalformedPostId ->
            ( { model
                | userNotices =
                    [ UN.routeNotFound Nothing ]
              }
            , Cmd.none
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
                    , Cmd.none
                    )
                    (\t ->
                        ( { model
                            | view = ViewTopic t
                          }
                        , "Discussions related to #"
                            ++ topic
                            ++ " on SmokeSignal"
                            |> Ports.setDescription
                        )
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
                        |> Dict.values
                        |> List.sortBy
                            (.core
                                >> Misc.sortPosts model.blockTimes model.now
                            )
                        |> List.map (.core >> .key)
                        |> List.Extra.greedyGroupsOf 10
                        |> Array.fromList
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
        (Misc.getConfig core.chain config)
        core.id.messageHash
        |> Task.attempt (PostAccountingFetched core.id)
    , if Dict.member core.id.block blockTimes then
        Cmd.none

      else
        Eth.getBlock
            (Misc.getProviderUrl core.chain config)
            core.id.block
            |> Task.map .timestamp
            |> Task.attempt (BlockTimeFetched core.id.block)
    ]
        |> Cmd.batch


addUserNotice :
    UserNotice
    -> Model
    -> Model
addUserNotice notice model =
    model
        |> addUserNotices [ notice ]


addUserNotices :
    List UserNotice
    -> Model
    -> Model
addUserNotices notices model =
    { model
        | userNotices =
            List.append
                model.userNotices
                notices
                |> List.Extra.uniqueBy .uniqueLabel
    }


logHttpError : String -> Http.Error -> Cmd msg
logHttpError tag =
    Misc.parseHttpError >> (++) (tag ++ ":\n") >> Ports.log


submitTxn : Model -> Chain -> TxSentry.CustomSend Msg -> Eth.Types.Send -> ( Model, Cmd Msg )
submitTxn model chain listeners txParams =
    case chain of
        Eth ->
            let
                ( newSentry, cmd ) =
                    TxSentry.customSend model.txSentry listeners txParams
            in
            ( { model
                | txSentry = newSentry
              }
            , cmd
            )

        XDai ->
            let
                ( newSentry, cmd ) =
                    TxSentry.customSend model.txSentryX listeners txParams
            in
            ( { model
                | txSentryX = newSentry
              }
            , cmd
            )


getPostBurnAmount : Float -> String -> Result String TokenValue
getPostBurnAmount price txt =
    if String.isEmpty txt then
        Ok TokenValue.zero

    else
        txt
            |> Misc.dollarStringToToken price
            |> Result.fromMaybe "Invalid burn amount"
