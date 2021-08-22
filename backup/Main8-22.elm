module Maintest exposing (..)

import Browser
import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (class, style)
import Html.Events.Extra.Mouse as Mouse
-- import Dict
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5)
-- import Svg
import Svg exposing (image)
import Svg.Attributes exposing (d, stroke, fill, strokeWidth, width, height, viewBox)
import Html exposing( img )
import Html.Attributes exposing ( src ) 

-- import Html.Attributes exposing (align)
import Html exposing (text)
import Html.Events exposing (custom, onClick)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox 
        { init = init
        , update = update
        , view = view
        }
            
-- MODEL 

type alias Magnitude = Float

type alias Position = (Magnitude, Magnitude)

type BrickType 
    = BasicBrick
    | EntryBrick
    | TailBrick
    | CaseBrick    

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
    = { getOnDnD : Bool                  -- DnDの最中は真 
      , getXY0 : Position  -- DnD開始時点のマウスの座標（event.offsetPos）
      }
      
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

brickSize : Magnitude
brickSize = 128

pallet : List (ASTxy Node)
pallet = [ASTxy --Aチーム
            (50, 200)
            ( ASTne
                { getBrickType = EntryBrick
                , getBrickCommand = CommandAteam
                }
                Nil
                Nil
            )
        , ASTxy --Bチーム
            (200, 200)
            ( ASTne 
                { getBrickType = EntryBrick
                , getBrickCommand = CommandBteam
                }
                Nil
                Nil
            )
        , ASTxy --直進
            (50, 350)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandMove
                }
                Nil
                Nil
            )
        , ASTxy --右
            (200, 350)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandTurnLeft
                }
                Nil
                Nil
            )
        , ASTxy --左
            (50, 500)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandTurnRight
                }
                Nil
                Nil
            )
        , ASTxy --音
            (200, 500)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandNoise
                }
                Nil
                Nil
            )
        , ASTxy --壁
            (50, 650)
            ( ASTne
                { getBrickType = CaseBrick
                , getBrickCommand = CommandGochin
                }
                Nil
                Nil
            ) 
        , ASTxy --傾き
            (200, 650)
            ( ASTne
                { getBrickType = CaseBrick
                , getBrickCommand = CommandKatamuki
                }
                Nil
                Nil
            )
        , ASTxy --光
            (50, 800)
            ( ASTne 
                { getBrickType = CaseBrick
                , getBrickCommand = CommandLight
                }
                Nil
                Nil
            )
        , ASTxy --A呼び出し
            (50, 950)
            ( ASTne
                { getBrickType = TailBrick
                , getBrickCommand = CommandAteamCall
                }
                Nil
                Nil
            ) 
        , ASTxy --A呼び出し
            (200, 950)
            ( ASTne
                { getBrickType = TailBrick
                , getBrickCommand = CommandBteamCall
                }
                Nil
                Nil
            )]

init : Model
init = 
    { getBrickSize = brickSize
    , getPallet = pallet
    , getASTRoots = []
    , getDnDInfo = 
        { getOnDnD = False
        , getXY0 = (0, 0)
        }
    } 

-- UPDATE

type Msg = MsgBrickMake (ASTne Node) Mouse.Event
         | MsgCloneUs (ASTxy Node)
         | MsgLetMeRoot (ASTne Node) Mouse.Event
         | MsgMoveUs Position Position 
         | MsgAttachMe (ASTxy Node) Mouse.Event
         | MsgStartDnD Mouse.Event         
         | MsgStartPUDnD (ASTxy Node) Mouse.Event

update : Msg -> Model -> Model
update msg = 
    case msg of
        --パレットに対してのみの複製、そのまま動かしたいためにstartDnDを呼び出す
        MsgBrickMake ast event -> 
            brickMake ast event >> startDnD event
        --それ以外に対しての複製、startDnDは不要、複製位置は右下にズラすように調整
        MsgCloneUs ast -> 
            cloneUs ast
        -- 根ではMsgLetMeRootは生じないので単独のMsgStartDnDは必要
        MsgStartDnD event ->
            startDnD event            
        MsgLetMeRoot ast event ->
            letMeRoot ast event >> startDnD event
        MsgMoveUs xy event ->
            moveUs xy event
        MsgAttachMe astxy event ->
            stopDnD event >> attachMe astxy event
        MsgStartPUDnD ast event ->
            startPUDnD ast event           

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


-- パレットに対してのみの複製
brickMake : ASTne Node -> Mouse.Event -> Model -> Model
brickMake ast event model =
    if event.button /= Mouse.MainButton && model.getDnDInfo.getOnDnD then
        model
    else
        addASTxy (ASTxy (pairMap2 (-) 
                          (pairMap2 (-) event.pagePos event.offsetPos) --凹凸を含む左上の角の座標
                          (19, 13)) -- 元のブロックと同じ位置に複製されるように位置調整
                        ast)
                 model

-- サブASTの複製を追加
-- 純粋関数型言語であるElmではあらゆるデータは不変（persistent）なので
-- 実際にはASTの複製をつくる必要がないことに注意
-- 単にクリックしたマウスの座標情報を付加した上で元のサブASTを共有するだけ
-- でよいので非常に効率的で軽い処理 
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


insideBrick : Position -> Position -> Bool
insideBrick (x0, y0) (x, y) = 
    y0 - mergin <= y && y <= y0 + mergin &&
    x0 - mergin <= x && x <= x0 + mergin

-- ドラッグアンドドロップ（DnD）の開始をモデルに記録
startDnD : Mouse.Event -> Model -> Model
startDnD event model = 
    if event.button /= Mouse.MainButton  then
        model
    else     
            { model | getDnDInfo = { getOnDnD = True, getXY0 = event.offsetPos } }
    
startPUDnD : ASTxy Node -> Mouse.Event -> Model -> Model
startPUDnD (ASTxy xy (ASTne node bottom right)) event model = 
    if event.button /= Mouse.MainButton  then
        model
    else     
        { model | getDnDInfo = { getOnDnD = True
                               , getXY0 = event.offsetPos
                               } }
        |> removeAST (ASTxy xy (ASTne node bottom right)) -- 一度掴んだブロックをリストから削除
        |> addASTxy (ASTxy xy (ASTne node bottom right))  -- 削除したブロックを再度作成する 


-- ドラッグアンドドロップ（DnD）の終了をモデルに記録
stopDnD : Mouse.Event -> Model -> Model
stopDnD event model =
    if event.button /= Mouse.MainButton  then
        model
    else   
        -- getXY0 = (0, 0)は不要だが，デバッガで見やすいように0をセット
        { model | getDnDInfo = { getOnDnD = False, getXY0 = (0, 0) } }

-- マウスでクリックした位置のブロックを根とするASTをモデルに追加する
-- 元のASTがすべて作り直される重い処理
-- 順序木への挿入処理のように本当は根に至る最小限の経路のみ作り直したいが，
-- クリック位置情報だけからはどちらの部分木の配下が更新されるのか判定できない
-- のでやむを得ない
letMeRoot : ASTne Node -> Mouse.Event -> Model -> Model
letMeRoot (ASTne node bottom right) event model = 
    if event.button /= Mouse.MainButton || model.getDnDInfo.getOnDnD 
    then
        model
    else     
        let
            -- 新規のルートポジション
            xy =  pairMap2 (-) event.pagePos event.offsetPos
                
            f xy0 n b r =
                if insideBrick xy0 xy then 
                    Nil -- 部分ASTをNilで置換
                else
                    AST n b r -- すべてのノードを作り直し
        in
            { model | getASTRoots = 
                model.getASTRoots 
                |> List.map (\(ASTxy (x, y) (ASTne n b r)) ->
                        let d = interval model 
                        in ASTxy (x, y)
                            (ASTne n
                                (recurAST d f (\_ -> Nil) (x, y + d) b)
                                (recurAST d f (\_ -> Nil) (x + d, y) r)
                            )
                    )
            }                                        
            |> addASTxy (ASTxy xy (ASTne node bottom right))   



-- 変更の有無を表す型
-- 構文木（のリスト）に関する変更操作は一度に一ヶ所に限られるので
-- 変更の有無を追跡して処理を効率化するのに広く用いる
type Change a
    = Changed a
    | Unchanged a

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
attachMe : ASTxy Node -> Mouse.Event -> Model -> Model
attachMe (ASTxy xy (ASTne node bottom right)) event model = 
    -- ドロップされたブロックがEntryBrickならアタッチしない（禁則）
    if node.getBrickType == EntryBrick ||
        event.button /= Mouse.MainButton then
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
        



preventDefaultOn : String -> Decoder msg -> Attribute msg
preventDefaultOn eventName =
    Decode.map (\m -> { message = m, stopPropagation = True, preventDefault = True })
        >> (custom eventName)
        
whenDragging : Model -> Decoder a -> Decoder a
whenDragging model decoder =
    Decode.when
        (Decode.succeed 0)
        (\_ -> model.getDnDInfo.getOnDnD)
        decoder

whenNotDragging : Model -> Decoder a -> Decoder a
whenNotDragging model decoder =
    Decode.when
        (Decode.succeed 0)
        (\_ -> not model.getDnDInfo.getOnDnD)
        decoder

whenRightButtonIsDown : Decoder a -> Decoder a 
whenRightButtonIsDown decoder =
    Decode.when
        (Decode.field "button" Decode.int)
        (\bt -> bt == 2)
        decoder

-- VIEW

view : Model -> Html Msg
view model =
    (model.getPallet
    |> List.indexedMap (\index astxy -> (String.fromInt index, viewPallet model astxy)))
    ++
    (model.getASTRoots
    |> List.indexedMap (\index astxy -> (String.fromInt index, viewASTxy model astxy)))
    |> Keyed.node "div" []


--パレットの描画
viewPallet : Model -> ASTxy Node -> Html Msg
viewPallet model (ASTxy (x, y) (ASTne n b r)) = 
    Keyed.node "div"
    [ style "position" "absolute"
    , style "top"  (String.fromFloat y ++ "px")
    , style "left" (String.fromFloat x ++ "px")
    , on "mousedown" (\event -> MsgBrickMake (ASTne n b r) event)
    ]
    [ ("N", brickSvg n model.getBrickSize ) -- 実際のブロックの描画はbrickSvgで
    ]  

-- 根のブロックの描画
viewASTxy : Model -> ASTxy Node -> Html Msg
viewASTxy model (ASTxy (x, y) (ASTne n b r)) = 
    Keyed.node "div"
    [ style "position" "absolute"
    , style "top"  (String.fromFloat y ++ "px")
    , style "left" (String.fromFloat x ++ "px")
    -- MsgLetMeRootは部分木に対してしか意味をなさないのでここで根にはセットしない
    -- 代わりにmousedownに対してはMsgStartDnDを単独でセット
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
    , on "mouseup"   (\event -> MsgAttachMe (ASTxy (x, y) (ASTne n b r))event)
    -- contextmenu
    -- コンテクストメニューが開かないようにpreventDefaultが必要
    , preventDefaultOn "contextmenu" 
        <| whenNotDragging model
            <| whenRightButtonIsDown
                <| Decode.succeed
                    <| MsgCloneUs (ASTxy ( x, y ) (ASTne n b r))
    , on "mousedown" (\event -> MsgStartPUDnD (ASTxy (x, y) (ASTne n b r)) event)
    ]
    [ ("N", brickSvg n model.getBrickSize ) -- 実際のブロックの描画はbrickSvgで
    , ("R", lazy3 viewAST model.getBrickSize True  r)
    , ("B", lazy3 viewAST model.getBrickSize False b)
    ]

-- 根以外の描画
viewAST: Magnitude -> Bool -> AST Node -> Html Msg
viewAST size isRight ast = 
    case ast of
        Nil ->
            div [] [] 
        AST node b r ->
            Keyed.node "div"
                [ style "position" "absolute"
                , style "top"  <| (if isRight then "0" else String.fromFloat (size * 0.9)) ++ "px"
                , style "left" <| (if isRight then String.fromFloat (size * 0.9) else "0") ++ "px"
                , on "mousedown" (\event -> MsgLetMeRoot (ASTne node b r) event)
                ]
                [ --("N", brickSvg size) 変更箇所
                  ("N", brickSvg node size ) -- 実際のブロックの描画はbrickSvgで
                , ("R", lazy3 viewAST size True  r)
                , ("B", lazy3 viewAST size False b)
                ]                        

-- ブリック要素は入れ子の木になっているので伝搬を止めないと複数のブリックが
-- イベントを複数ひろってしまうことに注意
on : String -> (Mouse.Event -> msg) -> Html.Attribute msg
on eventName =
    { stopPropagation = True, preventDefault = True }
        |> Mouse.onWithOptions eventName

-- 実際に各々のブロックを描く関数
brickSvg : Node -> Float -> Html Msg
brickSvg node size =
 let
    path =
        case node.getBrickType of
            EntryBrick -> "M 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 390 C 200 395.52285 204.47715 400 210 400 Z"
            BasicBrick -> "M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z"
            TailBrick -> "M 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 Z"
            CaseBrick -> "M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z"
    color =
        case node.getBrickType of
            EntryBrick -> "skyblue"
            BasicBrick -> "orange"
            TailBrick -> "pink"
            CaseBrick -> "lightgreen"
    image =
        case node.getBrickCommand of 
            CommandAteam -> "ase/Ateam.png"
            CommandBteam -> "ase/Bteam.png"
            CommandMove -> "ase/Go.png"
            CommandTurnRight -> "ase/Right.png"
            CommandTurnLeft -> "ase/left.png"
            CommandKatamuki -> "ase/katamuki.png"
            CommandGochin -> "ase/gochin.png"
            CommandNoise -> "ase/noise.png"
            CommandLight -> "ase/ligth.png"
            CommandAteamCall -> "ase/Ateam_call.png"
            CommandBteamCall -> "ase/Bteam_call.png"

 in
 
 div
    []
    -- [
    -- div[style "position" "negative"]
    [ img 
        [ src image 
        , width "92" 
        -- , height "100" 
        -- , align "center"
        , style "position" "absolute"
        , style "margin" "13px 19px"
        , class "image is-overray" -- Bulma必須
        , style "top" "0%"         -- Bulma必須
        , style "left" "0%"        -- Bulma必須
        ]
        []
    -- ]
    , Svg.svg
     [
       width  <| String.fromFloat size
    --  , height <| String.fromFloat size 
     , viewBox "166 70 336 336"
     ]
     [ Svg.path [ style "position" "absolute"
                , d path 
                , stroke "blue"
                , fill color
                , strokeWidth "6"
                ]
                []
     ] 
    ] 
 
