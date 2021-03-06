port module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, img, input, button, text)
import Html.Attributes exposing (src, class, style, value)
import Html.Events exposing (custom, targetValue, onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5)

import Svg exposing (image, path, svg)
import Svg.Attributes exposing (d, stroke, fill, strokeWidth, width, height, viewBox)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode

-- 外向きのメッセージ
port sendAST : Encode.Value -> Cmd msg

-- 内向きのメッセージ
port receiveMsg : (String -> msg) -> Sub msg


------------------------------------　MAIN  -------------------------------------------------

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- SUBSCRIPTIONS 
subscriptions : Model -> Sub Msg
subscriptions _ = receiveMsg MsgGet


-- MODEL --

-- 大きさ
type alias Magnitude = Float

-- ブロックの(x, y)座標
type alias Position = (Magnitude, Magnitude)

-- ブロックの接合方向 (ToRight:右側, ToBottom:左側)
type Direction
    = ToRight
    | ToBottom

-- ブロックの種類
type BrickType 
    = BasicBrick
    | EntryBrick
    | TailBrick
    | CaseBrick    

-- ブロックの命令
type BrickCommand 
    = CommandAteam --Aチーム
    | CommandBteam --Bチーム
    | CommandMove --まっすぐ
    | CommandTurnLeft --右
    | CommandTurnRight --左
    | CommandAteamCall --A呼び出し
    | CommandBteamCall --B呼び出し
    | CommandKatamuki --傾き
    | CommandGochin --ゴッちん
    | CommandNoise --音
    | CommandLight --光

type alias Node
    = { getBrickType : BrickType 
      , getBrickCommand : BrickCommand
      , getText : String
      }

-- 抽象構文木（AST）
type AST a
    = Nil
    | AST a
          (AST a) -- bottom (left)
          (AST a) -- right
-- a -> AST a -> AST a -> AST a

-- 非空（non-empty）のAST
type ASTne a
    = ASTne a (AST a) (AST a)
-- a -> AST a -> AST a -> ASTne a

-- 根の位置情報付きのAST
type ASTxy a
    = ASTxy 
        Position -- 根の座標
        (ASTne a)              -- 非空に限定 
-- Position -> ASTne a -> ASTxy a

type alias DnDInfo 
    = { getOnDnD : Bool    -- DnDの最中は真 
      , getXY0 : Position  -- DnD開始時点のマウスの座標
      }

-- モデルの情報
type alias Model 
    = { getBrickSize : Magnitude
      , getPallet : List (ASTxy Node)
      , getASTRoots : List (ASTxy Node) 
      , getDnDInfo: DnDInfo
      }

-- ブロック間の間隔
-- 凹凸の分を除くのでブロックサイズの90%
interval : Model -> Magnitude
interval model = model.getBrickSize * 0.9

-- 位置比較時のマージン
mergin : Magnitude
mergin = 20

-- ブロックの大きさ
brickSize : Magnitude
brickSize = 128

-- パレットデータ
pallet : List (ASTxy Node)
pallet = [ASTxy --Aチーム
            (50, 200)
            ( ASTne
                { getBrickType = EntryBrick
                , getBrickCommand = CommandAteam
                , getText = ""
                }
                Nil
                Nil
            )
        , ASTxy --Bチーム
            (200, 200)
            ( ASTne 
                { getBrickType = EntryBrick
                , getBrickCommand = CommandBteam
                , getText = ""
                }
                Nil
                Nil
            )
        , ASTxy --直進
            (50, 350)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandMove
                , getText = ""
                }
                Nil
                Nil
            )
        , ASTxy --右
            (200, 350)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandTurnLeft
                , getText = ""
                }
                Nil
                Nil
            )
        , ASTxy --左
            (50, 500)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandTurnRight
                , getText = ""
                }
                Nil
                Nil
            )
        , ASTxy --音
            (200, 500)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandNoise
                , getText = ""
                }
                Nil
                Nil
            )
        , ASTxy --壁
            (50, 650)
            ( ASTne
                { getBrickType = CaseBrick
                , getBrickCommand = CommandGochin
                , getText = ""
                }
                Nil
                Nil
            ) 
        , ASTxy --傾き
            (200, 650)
            ( ASTne
                { getBrickType = CaseBrick
                , getBrickCommand = CommandKatamuki
                , getText = ""
                }
                Nil
                Nil
            )
        , ASTxy --光
            (50, 800)
            ( ASTne 
                { getBrickType = CaseBrick
                , getBrickCommand = CommandLight
                , getText = ""
                }
                Nil
                Nil
            )
        , ASTxy --A呼び出し
            (50, 950)
            ( ASTne
                { getBrickType = TailBrick
                , getBrickCommand = CommandAteamCall
                , getText = ""
                }
                Nil
                Nil
            ) 
        , ASTxy --A呼び出し
            (200, 950)
            ( ASTne
                { getBrickType = TailBrick
                , getBrickCommand = CommandBteamCall
                , getText = ""
                }
                Nil
                Nil
            )]

init : () -> (Model, Cmd Msg)
init flags = (
    { getBrickSize = brickSize
    , getPallet = pallet
    , getASTRoots = []
    , getDnDInfo = 
        { getOnDnD = False
        , getXY0 = (0, 0)
        }
    }
    , Cmd.none )

------------------------------------　UPDATE  -------------------------------------------------

type Msg = MsgBrickMake (ASTxy Node) Position Position
         | MsgCloneUs (ASTxy Node)
         | MsgStartPUDnD Position Position
         | MsgLetMeRoot (ASTxy Node) Position Position
         | MsgMoveUs Position Position 
         | MsgAttachMe (ASTxy Node)
         | MsgInputChanged Position String
         | MsgDelete
         | MsgGet String
         | MsgDblClick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        --パレットに対してのみの複製、そのまま動かしたいためstartDnDを呼び出す
        MsgBrickMake ast xy1 xy0 -> 
            ( model |> brickMake ast |> startDnD xy1 xy0 , Cmd.none )
        --それ以外に対しての複製、startDnDは不要、複製位置は右下にズラすように調整
        MsgCloneUs ast -> 
            ( cloneUs ast model , Cmd.none )
        MsgStartPUDnD xy1 xy0 ->
            ( startPUDnD xy1 xy0 model , Cmd.none )
        -- 根ではMsgLetMeRootは生じないので単独のstartPUDnDは必要   
        MsgLetMeRoot ast xy1 xy0 ->
            ( model |> letMeRoot ast xy1 |> startPUDnD xy1 xy0 , Cmd.none )
        MsgMoveUs xy event ->
            ( moveUs xy event model , Cmd.none )
        MsgAttachMe astxy ->
            ( model |> stopDnD |> attachMe astxy , Cmd.none )
        MsgInputChanged xy text ->
            ( modifyText xy text model , Cmd.none )
        MsgDelete ->
            ( model , model |> deleteEncode |> sendAST )
        MsgGet command ->
            if command == "Delete" then
                ( delete model , Cmd.none )
            else 
                ( model , Cmd.none )
        MsgDblClick ->
            (model, model.getASTRoots |> astRootsEncode |> sendAST)


-- "Delete"メッセージをhtml側に送るためのEncode
deleteEncode : Model -> Encode.Value
deleteEncode model =
    Encode.string "Delete"
    

-- 場のブロックを全て削除する
delete : Model -> Model
delete model =
    { model | getASTRoots = []
    }   


-- ペア（2組）の各要素毎に2項演算を作用させる
-- 使用例： pairMap2 (+) (1, 10) (2, 20)  ==>  (3, 30)
pairMap2 : (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
pairMap2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)
    
-- 抽象構文木(AST)への再帰的処理のひな形
-- 以下の処理を葉から根にかけてボトムアップに適用する
-- - 葉には初期値を与える関数uを適用
-- - 葉以外のノードには以下の4引数の関数fを適用
-- f(d,n,b,r)の各引数の意味：
-- d: 各ブロック間の間隔
-- n: 葉以外の各ノード
-- b: サブAST bに対する処理済みの結果
-- r: サブAST rに対する処理済みの結果
-- 使用例： 
-- recurAST 0 (\_ a b c -> a + b + c) (\_ -> 0) (0,0) (AST 1 (AST 2 Nil Nil) (AST 3 Nil Nil)) 
-- ==> 6 (= 1 + 2 + 3)
recurAST : Magnitude -> 
           (Position -> a -> acc -> acc -> acc) -> 
           (Position -> acc) ->
           Position -> AST a -> acc
recurAST d f u (x, y) ast = 
    case ast of
        Nil -> 
            u (x, y)
        AST node bottom right ->     
            f (x, y)
              node
              (recurAST d f u (x, y + d) bottom)
              (recurAST d f u (x + d, y) right)

--ASTxyからASTneを取得する
getASTne : ASTxy Node -> ASTne Node
getASTne astxy =
         case astxy of
             ASTxy _ (ASTne node astb astr) ->
                 ASTne node astb astr
                 
--ASTneからNodeを取得する
getASTneNode : ASTne Node -> Node
getASTneNode astne =
            case astne of
                ASTne node _ _ ->
                    node
                    
-- モデルに（根の位置情報付きの）ASTを追加
addASTxy : ASTxy Node -> Model -> Model
addASTxy astxy model = 
        { model | getASTRoots = model.getASTRoots ++ [astxy] }

-- モデルから（根の位置情報付きの）ASTを削除
-- elmは参照透明なので等価性の判定は参照同値ではなく構造同値であることに注意
-- 実際は根の位置情報の食い違いで非等価がすぐに分かるのでAST全体を調べることはまれ
removeAST : ASTxy Node -> Model -> Model
removeAST astxy model =
    { model | getASTRoots = 
                model.getASTRoots |> List.filter (\a -> a /= astxy) 
    }    


-- サブASTの複製を追加
-- 純粋関数型言語であるElmではあらゆるデータは不変（persistent）なので
-- 実際にはASTの複製をつくる必要がないことに注意
-- 単にクリックしたマウスの座標情報を付加した上で元のサブASTを共有するだけ
-- でよいので非常に効率的で軽い処理
-- brickMakeはパレットに対してのみの複製
brickMake : ASTxy Node -> Model -> Model
brickMake (ASTxy ( x, y ) ast) model =
    model |> addASTxy (ASTxy (x, y) ast)

-- cloneUsはパレット以外に対しての複製
-- 複製時にブロックの座標を少し右下にずらす
cloneUs : ASTxy Node -> Model -> Model
cloneUs (ASTxy ( x, y ) ast) model =
    model |> addASTxy (ASTxy (x + 10, y + 10) ast)


-- ASTの移動
-- 単に根の位置情報を変更するだけでよいので非常に軽い処理
moveUs : Position -> Position -> Model -> Model
moveUs ( x, y ) ( dx, dy ) model =
    { model
        | getASTRoots =
            model.getASTRoots
                |> List.map
                    (\(ASTxy p a) ->
                        if p == ( x, y ) then
                            -- DnD開始時のoffsetPosと現在のoffsetPosの差が移動量
                            ASTxy ( x + dx, y + dy ) a
                        else
                            ASTxy p a
                    )
    }




-- ドラッグアンドドロップ（DnD）の開始をモデルに記録
startDnD : Position -> Position -> Model -> Model
startDnD xy1 xy0 model =
    { model | getDnDInfo = { getOnDnD = True, getXY0 = xy0 } }

-- クリックしたブロックをASTリストの最後に並び替える用のstartDnD
startPUDnD : Position -> Position -> Model -> Model
startPUDnD xy1 xy0 model =
    -- offsetPosはサブコンポーネント（画像、入力フィールド）基準であるため注意
    { model | getDnDInfo = { getOnDnD = True, getXY0 = xy0 },
              getASTRoots =
                  let
                    nonDnDRoots =
                        model.getASTRoots
                            |> List.filter (\(ASTxy p _) -> p /= xy1)
                    theDnDRoot =
                        model.getASTRoots
                            |> List.filter (\(ASTxy p _) -> p == xy1)
                  in
                      -- 選択（mousedown）された木がASTのリストの最後にくるように並べ替え
                      -- これによりブロックが重なっても選択されたものが常に最前面に表示されるようになり
                      -- DnDの途中で重なった他のブロックにフォーカスをとられるバグに対処
                      nonDnDRoots ++ theDnDRoot
    }


-- ドラッグアンドドロップ（DnD）の終了をモデルに記録
stopDnD : Model -> Model
stopDnD model =
    -- getXY0 = (0, 0)は不要だが，デバッガで見やすいように0をセット
    { model | getDnDInfo = { getOnDnD = False, getXY0 = ( 0, 0 ) } }


insideBrick : Position -> Position -> Bool
insideBrick (x0, y0) (x, y) = 
    y0 - mergin <= y && y <= y0 + mergin &&
    x0 - mergin <= x && x <= x0 + mergin

-- マウスでクリックした位置のブロックを根とするASTをモデルに追加する
-- 以前の実装では常にすべての木のすべてのノードを巡回する重い操作だったが
-- Change型を使い、直接再帰で書き下すことで効率化
letMeRoot : ASTxy Node -> Position -> Model -> Model
letMeRoot (ASTxy xy (ASTne node bottom right)) ( x, y ) model =
    let
        d = interval model

        -- 以前の版では再帰のテンプレートrecurASTを呼び出していたが、
        -- elmはCVBなので(thunkを用いない限り)不要な再帰呼び出しも実行されてしまう
        -- これはChange型の導入だけでは防ぎきれない
        rec xy0 ast =
            case ast of
                Nil ->
                    Unchanged Nil
                AST n b r ->
                    -- 次の行で ( x, y )をxyにするとMsgModifyTxtが発生しなくなる（未解決）
                    -- xyはクリックされたブロックの左上座標だが、( x, y )はサブコンポーネントの
                    -- 左上の座標かもしれない点にも注意
                    if insideBrick xy0 ( x, y ) then
                        Changed Nil  -- 部分ASTをNilで置換
                    else
                        Unchanged asAST
                            |> andMapC (spawn d rec rec xy0 (ASTne n b r))
        newRoots = 
            model.getASTRoots
                |> mapOnce
                    (\(ASTxy xy0 astne) ->
                        Unchanged (ASTxy xy0)
                            |> andMapC (spawn d rec rec xy0 astne)
                    )
    in
        -- 小森・山田くんたちの成果の取り入れ
        -- ブロックにtext inputのようなサブコンポーネントを追加してこれをクリックした場合
        -- サブコンポーネントのoffsetX/Yが送られることになり、letMeRootが失敗する可能性が生じる．
        -- そこでletMeRootが成功したことを確認した上でaddASTxyするように修正
        case newRoots of
            Changed roots ->
                { model | getASTRoots = roots }
                    |> addASTxy (ASTxy xy (ASTne node bottom right))
            Unchanged _ ->
                model



-- 変更の有無を表す型
-- 構文木（のリスト）に関する変更操作は一度に一ヶ所に限られるので
-- 変更の有無を追跡して処理を効率化するのに広く用いる
type Change a
    = Changed a
    | Unchanged a

-- 不要になった変更フラグを剥がす
discardC : Change a -> a
discardC ca =
    case ca of
        Changed a ->
            a
        Unchanged a ->
            a

-- ASTneのASTへのキャスト（読み替え）
asAST : ASTne a -> AST a
asAST (ASTne n b r) =
    AST n b r

-- Change型の値を返す関数fをリストxsの各要素に適用
-- 変更があれば残りの要素への変更はスキップして効率化
-- 使用例:
-- mapOnce absC [1, -2, 3, -4, 5] ==> Changed [1, 2, 3, -4, 5]
mapOnce : (a -> Change a) -> List a -> Change (List a)
mapOnce f xs =
    case xs of
        [] ->
            Unchanged []
        x :: xs_ ->
            case f x of
                Changed a ->
                    Changed (a :: xs_)
                Unchanged _ ->
                    Unchanged (\xs__ -> x :: xs__)
                        -- Haskellと違いelmは(x::)と書けない
                        |> andMapC (mapOnce f xs_)

-- Change型に対する適用的関手（多引数版のmapを実現）
-- 使用例:
-- absC x = if x > 0 then (Unchanged x) else (Changed -x)
-- Unchanged (+) |> andMapC (absC 1) |> andMapC (absC -2)  ==> (Changed 3)
-- Unchanged (+) |> andMapC (absC 1) |> andMapC (absC 2)   ==> (Unchanged 3)
andMapC : Change a -> Change (a -> b) -> Change b
andMapC ca cf =
    case ( ca, cf ) of
        ( Changed a, Changed f ) ->
            Changed (f a)
        ( Changed a, Unchanged f ) ->
            Changed (f a)
        ( Unchanged a, Changed f ) ->
            Changed (f a)
        ( Unchanged a, Unchanged f ) ->
            Unchanged (f a)

-- 関数funB, funRを非空ASTの両方（直下、直右）の部分木に対してそれぞれ呼び出す
-- このときChange型の情報を利用して必要なければ呼び出しをスキップ
spawn :
    Magnitude
    -> (Position -> AST a -> Change (AST a))
    -> (Position -> AST a -> Change (AST a))
    -> Position
    -> ASTne a
    -> Change (ASTne a)
spawn d funB funR ( x, y ) (ASTne n b r) =
    case funB ( x, y + d ) b of
        Changed newB ->
            Unchanged (ASTne n)
                |> andMapC (Changed newB)
                |> andMapC (Unchanged r)
        Unchanged _ ->
            Unchanged (ASTne n)
                |> andMapC (Unchanged b)
                |> andMapC (funR ( x + d, y ) r)

-- モデルから（根の位置情報付きの）ASTを削除
-- elmは参照透明なので等価性の判定は参照同値ではなく構造同値であることに注意
-- 実際は根の位置情報の食い違いで非等価がすぐに分かるのでAST全体を調べることはまれ
removeASTxy : ASTxy Node -> Model -> Model
removeASTxy astxy model =
    { model
        | getASTRoots =
            model.getASTRoots |> List.filter (\a -> a /= astxy)
    }

-- ASTを別のASTの葉に接木する
-- 以前の実装では常にすべての木のすべてのノードを巡回する重い操作だったが
-- Change型を使い、直接再帰で書き下すことで効率化
attachMe : ASTxy Node -> Model -> Model
attachMe (ASTxy xy (ASTne node bottom right)) model =
    -- ブロックが左端で放されたら消える機能
    if  (Tuple.first xy) < 370 then 
        model |> removeASTxy (ASTxy xy (ASTne node bottom right))
    -- ドロップされたブロックがEntryBrickならアタッチしない（禁則）
    else if node.getBrickType == EntryBrick  then
        model
    else
        let
            d =
                interval model

            -- ブロックnの直下へのアタッチが許されるかどうかを
            -- ブロックnの種類と他方の子ノードaの情報を用いて判定
            isAttachable n a =
                n.getBrickType == CaseBrick ||
                n.getBrickType /= TailBrick && (a == Nil) 
                

            -- 以前の版では再帰のテンプレートrecurASTを呼び出していたが、
            -- elmはCVBなので(thunkを用いない限り)不要な再帰呼び出しも実行されてしまう
            -- これはChange型の導入だけでは防ぎきれない
            -- アタッチが許容されるかどうかの判定は親ノードでおこなうので
            -- 判定結果を引数attachableで子ノードに渡している
            rec attachable xy0 ast =
                case ast of
                    Nil ->
                        -- 以前の版では親ノードでアタッチを実行するように一旦変更したが
                        -- 再びNilノードで行うように戻した
                        -- アタッチ許容位置でクリックされたときにはアタッチを実行
                        if attachable && insideBrick xy0 xy then
                            Changed (AST node bottom right)
                        else
                            Unchanged Nil
                    AST n b r ->
                        Unchanged asAST
                            |> andMapC
                                (spawn d
                                    (rec (isAttachable n r))
                                    (rec (isAttachable n b))
                                    xy0 
                                    (ASTne n b r)
                                )

            newRoots =
                model.getASTRoots
                    |> mapOnce
                        (\(ASTxy xy0 (ASTne n b r)) ->
                            Unchanged (ASTxy xy0)
                                |> andMapC
                                    (spawn d
                                        (rec (isAttachable n r))
                                        (rec (isAttachable n b))
                                        xy0 
                                        (ASTne n b r)
                                    )
                        )
        in
        -- 接木が成功したことを確認したときだけリストから削除する
        case newRoots of
            Changed roots ->
                { model | getASTRoots = roots }
                    |> removeASTxy (ASTxy xy (ASTne node bottom right))
            Unchanged _ ->
                model


-- text を　modify(変更)
modifyText : Position -> String -> Model -> Model
modifyText xy newText model =
    let
            d = interval model

            rec xy0 ast =
                case ast of
                    Nil ->
                        Unchanged Nil
                    AST n b r ->
                        if insideBrick xy0 xy then
                            Changed (AST { n | getText = newText } b r)  
                        else
                            Unchanged asAST
                                |> andMapC (spawn d rec rec xy0 (ASTne n b r))

        in
            { model
                | getASTRoots =
                    model.getASTRoots
                        |> mapOnce
                            (\(ASTxy xy0 (ASTne n b r)) ->
                                if insideBrick xy0 xy then
                                    Changed (ASTxy xy0 (ASTne { n | getText = newText } b r))
                                else
                                    Unchanged (ASTxy xy0)
                                        |> andMapC (spawn d rec rec xy0 (ASTne n b r))
                            )
                        |> discardC
            }


------------------------------------　VIEW  -------------------------------------------------


-- ブリック要素は入れ子の木になっているので伝搬を止めないと複数のブリックが
-- イベントを複数ひろってしまうので、stepPropagationは常にTrue
on : String -> Decoder msg -> Attribute msg
on eventName =
    Decode.map (\m -> { message = m, stopPropagation = True, preventDefault = False })
        >> (custom eventName)

-- 事前にonであるかを確認
preventDefaultOn : String -> Decoder msg -> Attribute msg
preventDefaultOn eventName =
    Decode.map (\m -> { message = m, stopPropagation = True, preventDefault = True })
        >> (custom eventName)

-- 左クリック時
whenLeftButtonIsDown : Decoder a -> Decoder a 
whenLeftButtonIsDown decoder =
    Decode.when
        (Decode.field "button" Decode.int)
        (\bt -> bt == 0)
        decoder

-- 右クリック時
whenRightButtonIsDown : Decoder a -> Decoder a 
whenRightButtonIsDown decoder =
    Decode.when
        (Decode.field "button" Decode.int)
        (\bt -> bt == 2)
        decoder

-- ドラッグ中
whenDragging : Model -> Decoder a -> Decoder a
whenDragging model decoder =
    Decode.when
        (Decode.succeed 0)
        (\_ -> model.getDnDInfo.getOnDnD)
        decoder

-- 非ドラッグ中
whenNotDragging : Model -> Decoder a -> Decoder a
whenNotDragging model decoder =
    Decode.when
        (Decode.succeed 0)
        (\_ -> not model.getDnDInfo.getOnDnD)
        decoder


-- メインのview
-- パレット部分とブロック操作部分を同時に描画している
view : Model -> Html Msg
view model =
    (model.getPallet
    |> List.indexedMap (\index astxy -> (String.fromInt index, viewPallet model astxy)))
    ++
    (model.getASTRoots
    |> List.indexedMap (\index astxy -> (String.fromInt index, viewASTxy model astxy)))
    |> Keyed.node "div" []


--パレット部分の描画
viewPallet : Model -> ASTxy Node -> Html Msg
viewPallet model (ASTxy (x, y) (ASTne n b r)) = 
    Keyed.node "div"
        [ style "position" "absolute"
        , style "top"  (String.fromFloat y ++ "px")
        , style "left" (String.fromFloat x ++ "px")
        -- mousedown
        , on "mousedown"
            <| whenNotDragging model
                <| whenLeftButtonIsDown
                    <| (Decode.succeed (\x0 y0 x1 y1 -> 
                    -- パレットに対しての左クリックには、ブロックを複製するMsgBrickMakeを呼び出す
                                       MsgBrickMake (ASTxy ( x, y ) (ASTne n b r)) 
                                                     ( x1 - x0, y1 - y0 )
                                                     ( x0, y0 ))
                                        |> Decode.andMap (Decode.field "offsetX" Decode.float)
                                        |> Decode.andMap (Decode.field "offsetY" Decode.float)
                                        |> Decode.andMap (Decode.field "pageX" Decode.float)
                                        |> Decode.andMap (Decode.field "pageY" Decode.float))
        ]
        [ ("N", lazy3 viewBrick model ( x, y ) n ) -- 実際のブロックの描画はbrickSvgで
        ]  



-- 根のブロックの描画
viewASTxy : Model -> ASTxy Node -> Html Msg
viewASTxy model (ASTxy (x, y) (ASTne n b r)) = 
    Keyed.node "div"
        [ style "position" "absolute"
        , style "top"  (String.fromFloat y ++ "px")
        , style "left" (String.fromFloat x ++ "px")
        -- mouseup
        , on "mouseup"
              <| whenDragging model
                     <| whenLeftButtonIsDown 
                            <| Decode.succeed
                                   <| MsgAttachMe (ASTxy ( x, y ) (ASTne n b r))
        -- mousedown
        , on "mousedown"
            <| whenNotDragging model
                <| whenLeftButtonIsDown
                -- MsgLetMeRootは根には無意味、代わりにMsgStartPUDnDを単独でセット
                    (Decode.succeed (\x1 y1 -> MsgStartPUDnD ( x, y ) ( x1, y1 ))  --( x, y ) を追加
                        |> Decode.andMap (Decode.field "offsetX" Decode.float)
                        |> Decode.andMap (Decode.field "offsetY" Decode.float))    
    -- mousemove
    -- ブロック表面の画像だけがドラッグされないようにpreventDefaultが必要
        , preventDefaultOn "mousemove"
            <| whenDragging model
                    <| let 
                            ( x0, y0 ) = model.getDnDInfo.getXY0
                       in
                            Decode.succeed (\x1 y1 -> MsgMoveUs ( x, y ) ( x1 - x0, y1 - y0 )) 
                                |> Decode.andMap (Decode.field "offsetX" Decode.float)
                                |> Decode.andMap (Decode.field "offsetY" Decode.float)
    -- contextmenu
    -- コンテクストメニューが開かないようにpreventDefaultが必要
        , preventDefaultOn "contextmenu" 
            <| whenNotDragging model
                <| whenRightButtonIsDown
                    <| Decode.succeed
                        <| MsgCloneUs (ASTxy ( x, y ) (ASTne n b r))
        ]
        [ ("N", lazy3 viewBrick model ( x, y ) n ) -- 実際のブロックの描画はviewBrickで
        , ("R", lazy4 viewAST model ( x + interval model, y ) ToRight r )
        , ("B", lazy4 viewAST model (x, y + interval model ) ToBottom b )
        ]


-- 根以外の木の再帰的描画
viewAST : Model -> Position -> Direction -> AST Node -> Html Msg
viewAST model ( x, y ) direction ast =
    case ast of
        Nil ->
            div [] []
        AST n b r ->
            Keyed.node "div"
                [ style "position" "absolute"
                , style "top" <|
                    case direction of
                        ToRight ->
                            "0px" 
                        ToBottom ->
                            String.fromFloat (interval model) ++ "px"
                , style "left" <|
                    case direction of
                        ToBottom ->
                            "0px" 
                        ToRight ->
                            String.fromFloat (interval model) ++ "px"
                -- mousedown
                , on "mousedown"
                      <| whenNotDragging model
                             <| whenLeftButtonIsDown
                                    (Decode.succeed (\x0 y0 x1 y1 -> 
                                        MsgLetMeRoot (ASTxy ( x, y ) (ASTne n b r)) 
                                                     ( x1 - x0, y1 - y0 )
                                                     ( x0, y0 ))
                                        |> Decode.andMap (Decode.field "offsetX" Decode.float)
                                        |> Decode.andMap (Decode.field "offsetY" Decode.float)
                                        |> Decode.andMap (Decode.field "pageX" Decode.float)
                                        |> Decode.andMap (Decode.field "pageY" Decode.float))
                -- contextmenu
                -- コンテクストメニューが開かないようにpreventDefaultが必要
                , preventDefaultOn "contextmenu" 
                      <| whenNotDragging model
                             <| whenRightButtonIsDown
                                    <| Decode.succeed
                                           <| MsgCloneUs (ASTxy ( x, y ) (ASTne n b r))
                ]
                [ ( "N", lazy3 viewBrick model ( x, y ) n ) -- 実際のブロックの描画はbrickSvgで
                , ( "R", lazy4 viewAST model ( x + interval model, y ) ToRight r )
                , ( "B", lazy4 viewAST model ( x, y + interval model ) ToBottom b )
                ]


-- 実際に各々のブロックを描く関数
viewBrick : Model -> Position -> Node -> Html Msg
viewBrick model xy node =
    let
        -- ブロックの形を指定
        viewBrickPath =
            case node.getBrickType of
                BasicBrick ->
                    "M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z"
                EntryBrick ->
                    "M 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 390 C 200 395.52285 204.47715 400 210 400 Z"
                TailBrick ->
                    "M 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 Z"
                CaseBrick ->
                    "M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z"
        -- ブロックの色を指定
        color =
            case node.getBrickType of
                BasicBrick ->
                    "orange"
                EntryBrick ->
                    "skyblue"
                TailBrick ->
                    "pink"
                CaseBrick ->
                    "lightgreen"
        -- ブロック上の画像を指定
        image =
            case node.getBrickCommand of 
                CommandAteam -> 
                    "ase/Ateam.png"
                CommandBteam -> 
                    "ase/Bteam.png"
                CommandMove -> 
                    "ase/Go.png"
                CommandTurnRight -> 
                    "ase/Right.png"
                CommandTurnLeft -> 
                    "ase/left.png"
                CommandKatamuki -> 
                    "ase/katamuki.png"
                CommandGochin -> 
                    "ase/gochin.png"
                CommandNoise -> 
                    "ase/noise.png"
                CommandLight -> 
                    "ase/ligth.png"
                CommandAteamCall -> 
                    "ase/Ateam_call.png"
                CommandBteamCall -> 
                    "ase/Bteam_call.png"
    in
    div
        []
        [ svg
            [ width  <| String.fromFloat model.getBrickSize
            , height <| String.fromFloat model.getBrickSize 
            , viewBox "166 70 336 336"
            ]
            [ path
                [ d viewBrickPath
                , stroke "blue"
                , fill color
                , strokeWidth "6"
                ]
                []
            ]
        , img
            [ src image
            , width "92"
            , style "position" "absolute"
            , style "margin" "13px 19px"
            , class "image is-overray" -- Bulma必須
            , style "top" "0%" -- Bulma必須
            , style "left" "0%" -- Bulma必須(topと同じなら無くても可)
            ]
            []
        , input 
            [ style "position" "absolute"
            , style "top" "65%" 
            , style "left" "40%" 
            , style "width" "35px"
            , value <| node.getText
            -- input
            , (Decode.map (MsgInputChanged xy) targetValue) |> on "input"
            ] []
        ]



-- MISC
-- Html側にブロックや操作などのデータを送信する用

brickTypeEncode : BrickType -> Encode.Value
brickTypeEncode brickType =
    case brickType of
        BasicBrick ->
            Encode.string "BasicBrick"
        EntryBrick ->
            Encode.string "EntryBrick"
        TailBrick ->
            Encode.string "TailBrick"
        CaseBrick ->
            Encode.string "CaseBrick"


brickCommandEncode : BrickCommand -> Encode.Value
brickCommandEncode brickCommand =
    case brickCommand of
        CommandAteam ->
            Encode.string "CommandAteam"
        CommandBteam ->
            Encode.string "CommandBteam"
        CommandMove ->
            Encode.string "CommandMove"
        CommandTurnLeft ->
            Encode.string "CommandTurnLeft"
        CommandTurnRight ->
            Encode.string "CommandTurnRight"
        CommandAteamCall ->
            Encode.string "CommandAteamCall"
        CommandBteamCall ->
            Encode.string "CommandBteamCall"
        CommandKatamuki ->
            Encode.string "CommandKatamuki"
        CommandGochin ->
            Encode.string "CommandGochin"
        CommandNoise ->
            Encode.string "CommandNoise"
        CommandLight ->
            Encode.string "CommandLight"


nodeEncode : Node -> Encode.Value
nodeEncode { getBrickType, getBrickCommand, getText } =
    Encode.object
        [ ("getBrickType", brickTypeEncode getBrickType)
        , ("getBrickCommand", brickCommandEncode getBrickCommand)
        , ("getText", Encode.string getText)
        ]

astEncode : AST Node -> Encode.Value
astEncode ast =
    case ast of
        Nil ->
            Encode.null  -- JSON null値
        AST node left right ->
            Encode.object
                [ ("node", nodeEncode node)
                , ("left", astEncode left)
                , ("right", astEncode right)
                ]

astneEncode : ASTne Node -> Encode.Value
astneEncode (ASTne node left right) =
    Encode.object
        [ ("node", nodeEncode node)
        , ("left", astEncode left)
        , ("right", astEncode right)
        ]

astxyEncode : ASTxy Node -> Encode.Value
astxyEncode (ASTxy _ astne) =
    astneEncode astne

astRootsEncode : List (ASTxy Node) -> Encode.Value
astRootsEncode roots =
    Encode.list astxyEncode roots
