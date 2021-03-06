module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style) 
import Html.Events.Extra.Mouse as Mouse
import Svg
import Svg.Attributes exposing (d, stroke, fill, strokeWidth, width, height, viewBox)
import Dict
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5)
import Svg exposing (image)
import Html exposing( img )
import Html.Attributes exposing ( src ) 

import Html.Attributes exposing (align)
import Html exposing (text)
import Html.Events exposing (onClick)

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
            (50, 0)
            ( ASTne
                { getBrickType = EntryBrick
                , getBrickCommand = CommandAteam
                }
                Nil
                Nil
            )
        , ASTxy --Bチーム
            (200, 0)
            ( ASTne 
                { getBrickType = EntryBrick
                , getBrickCommand = CommandBteam
                }
                Nil
                Nil
            )
        , ASTxy --直進
            (50, 150)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandMove
                }
                Nil
                Nil
            )
        , ASTxy --右
            (200, 150)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandTurnLeft
                }
                Nil
                Nil
            )
        , ASTxy --左
            (350, 150)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandTurnRight
                }
                Nil
                Nil
            )
        , ASTxy --音
            (500, 150)
            ( ASTne 
                { getBrickType = BasicBrick
                , getBrickCommand = CommandNoise
                }
                Nil
                Nil
            )
        , ASTxy --壁
            (50, 300)
            ( ASTne
                { getBrickType = CaseBrick
                , getBrickCommand = CommandGochin
                }
                Nil
                Nil
            ) 
        , ASTxy --傾き
            (200, 300)
            ( ASTne
                { getBrickType = CaseBrick
                , getBrickCommand = CommandKatamuki
                }
                Nil
                Nil
            )
        , ASTxy --光
            (350, 300)
            ( ASTne 
                { getBrickType = CaseBrick
                , getBrickCommand = CommandLight
                }
                Nil
                Nil
            )
        , ASTxy --A呼び出し
            (50, 450)
            ( ASTne
                { getBrickType = TailBrick
                , getBrickCommand = CommandAteamCall
                }
                Nil
                Nil
            ) 
        , ASTxy --A呼び出し
            (200, 450)
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

type Msg = MsgCloneUs (ASTne Node) Mouse.Event
         | MsgLetMeRoot (ASTne Node) Mouse.Event
         | MsgMoveUs Position Mouse.Event
         | MsgAttachMe (ASTxy Node) Mouse.Event
         | MsgStartDnD Mouse.Event         

update : Msg -> Model -> Model
update msg = 
    case msg of
        MsgCloneUs ast event -> 
            cloneUs ast event
        -- 根ではMsgLetMeRootは生じないので単独のMsgStartDnDは必要
        MsgStartDnD event ->
            startDnD event            
        MsgLetMeRoot ast event ->
            letMeRoot ast event >> startDnD event
        MsgMoveUs xy event ->
            moveUs xy event
        MsgAttachMe astxy event ->
            stopDnD event >> attachMe astxy event

-- ASTへの処理で更新があったかどうかの判定で使用
type Change = Changed | Unchanged

cor : Change -> Change -> Change
cor a b = 
    case (a, b) of
        (Unchanged, Unchanged) -> 
            Unchanged
        (_ ,_ ) ->
            Changed   

-- Writerアプリカティブ
-- elmには型クラスがないのでChangeモノイドに特化
type alias W a = (a, Change)

-- 使用例: 
-- absW x = if x > 0 then (x, Unchanged) else (-x, Changed)
-- unit (+) |> andMap (absW 1) |> andMap (absW -2)  ==> (3, Changed)
-- unit (+) |> andMap (absW 1) |> andMap (absW 2)   ==> (3, Unchanged)
andMap : W a -> W (a -> b) -> W b
andMap (a, c) (f, d) = (f a, cor c d)

unit : a -> W a
unit a = (a, Unchanged)

-- リストの各要素に関数fを作用させ（List.mapと同じ），その過程で
-- 変更が生じたかどうかを合わせて返す
-- 使用例：
-- absW x = if x > 0 then (x, Unchanged) else (-x, Changed)
-- listMapW absW [1, -2, 3, -4, 5]  ==>  ([1, 2, 3, 4, 5], Changed)
-- listMapW absW [1, 2, 3, 4, 5]    ==>  ([1, 2, 3, 4, 5], Unchanged)
listMapW : (a -> W a) -> List a -> W (List a) 
listMapW f = List.foldr (\a mbs -> unit (::) |> andMap (f a) |> andMap mbs)
                        (unit [])           

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
cloneUs : ASTne Node -> Mouse.Event -> Model -> Model
cloneUs ast event model =
    if event.button == Mouse.MainButton || model.getDnDInfo.getOnDnD then
        model
    else
        addASTxy (ASTxy (pairMap2 (+) 
                          (pairMap2 (-) event.pagePos event.offsetPos) --凹凸を含む左上の角の座標
                          (10, 10)) -- 元のブロックと完全に重ならないように少しずらす
                        ast)
                 model    

-- ASTの移動
-- 単に根の位置情報を変更するだけでよいので非常に軽い処理
moveUs : Position -> Mouse.Event -> Model -> Model
moveUs xy event model = 
    if not model.getDnDInfo.getOnDnD then 
        -- MsgAttachMeの直後にMsgMoveUsが生じてブリックが不作為にリープするバグへの対処
        -- おそらくメッセージがキューされていて，タイミングが悪いとDnD終了後であっても
        -- MsgAttachMeが生じるものと思われる．
        model
    else    
        { model | getASTRoots = 
            model.getASTRoots 
            |> List.map(\(ASTxy p a) -> 
                            if p == xy then
                                ASTxy 
                                    (pairMap2 (+) 
                                        xy 
                                        (pairMap2 (-) 
                                            event.offsetPos 
                                            model.getDnDInfo.getXY0 
                                        )
                                    )
                                    a
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
    
-- ASTを別のASTの葉に接木する
-- かならずしも接木が行われるとは限らない（近接ブロックがない位置でマウスをリリースするかもしれない）
-- よって接木が成功したことを確認した上ではじめてAST astをASTのリストから削除すべきことに注意
-- letMeRootと同様，すべてのASTを作り直すことになる重い処理（接木したい位置情報からだけでは
-- どちらの部分木配下が対象になるのか判定しようがないので致し方ない）
attachMe : ASTxy Node -> Mouse.Event -> Model -> Model
attachMe (ASTxy xy (ASTne node bottom right)) event model = 
    let 
        f top xy0 n b r =
 --           unit (AST n) |> andMap b |> andMap r
            if b == (Nil, Unchanged) && insideBrick xy0 (Tuple.first xy , Tuple.second xy - brickSize) then
                    unit (top n) |> andMap (AST node bottom right, Changed) |> andMap r
            else 
            if r == (Nil, Unchanged) && insideBrick xy0 (Tuple.first xy - brickSize , Tuple.second xy) then
                    unit (top n) |> andMap b |> andMap (AST node bottom right, Changed)
                else
                    unit (top n) |> andMap b |> andMap r
    in    
        if event.button /= Mouse.MainButton 
        || node.getBrickType == EntryBrick
        then
            model
        else
            let 
                (newRoots, isChanged) = 
                    model.getASTRoots
                    |> listMapW (\(ASTxy (x, y) (ASTne n b r)) ->
                         --if n.getBrickType /= TailBrick then --追加、根のBrickTypeがTailで無いことを判定する
                            let
                                d = interval model
                            in 
                                     (andMap
                                            (f ASTne (x, y) n
                                            (recurAST d (f AST) (\_ -> unit Nil) (x, y + d) b)
                                            (recurAST d (f AST) (\_ -> unit Nil) (x + d, y) r) 
                                            )
                                            (unit (ASTxy (x, y)))
                                        )
                              --else unit (ASTxy (x, y) (ASTne n b r)) --追加、根のBrickTypeがTailの場合何も変更を加えない処理を行う
                                  
                         )
                newModel = 
                    { model | getASTRoots = newRoots }
            in
                -- 接木が成功したことを確認したときだけリストから削除する
                if isChanged == Changed then
                    newModel |> removeAST (ASTxy xy (ASTne node bottom right))
                else    
                    newModel


-- VIEW

view : Model -> Html Msg
view model =
    ((model.getPallet)++(model.getASTRoots))
    |> List.indexedMap (\index astxy -> (String.fromInt index, viewASTxy model astxy))
    |> Keyed.node "div" [] 

-- 根のブロックの描画
viewASTxy : Model -> ASTxy Node -> Html Msg
viewASTxy model (ASTxy (x, y) (ASTne n b r)) = 
    Keyed.node "div"
    [ style "position" "absolute"
    , style "top"  (String.fromFloat y ++ "px")
    , style "left" (String.fromFloat x ++ "px")
    -- MsgLetMeRootは部分木に対してしか意味をなさないのでここで根にはセットしない
    -- 代わりにmousedownに対してはMsgStartDnDを単独でセット
    , on "mousemove" (\event -> MsgMoveUs (x, y) event)
    , on "mouseup"   (\event -> MsgAttachMe (ASTxy (x, y) (ASTne n b r)) event)
    , on "contextmenu" (\event -> MsgCloneUs (ASTne n b r) event)
    , on "mousedown" (\event -> MsgStartDnD event) 
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
                , on "contextmenu" (\event -> MsgCloneUs   (ASTne node b r) event)
                , on "mousedown"   (\event -> MsgLetMeRoot (ASTne node b r) event)
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

 
 div[][
    div[style "position" "absolute"][ img [ src image , width "130" , height "130" ,align "center"][]]
    ,
    Svg.svg
     [
      width  <| String.fromFloat size
     , height <| String.fromFloat size 
     , viewBox "166 70 336 336"
     ]
     [ Svg.path [style "position" "absolute"
                ,d path 
                , stroke "blue"
                , fill color
                ,strokeWidth "6"
                ][]
     ] 
 ] 