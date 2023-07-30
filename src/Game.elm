module Game exposing (..)

import Browser
import Html.Events 
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Types exposing (..)
import LightTools exposing (..)
import Dict exposing (Dict)
import Random

main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

size = 3 -- if the size is an even integer and p >2, then the laplacian is invertible.
p = 3
init: () -> (Model, Cmd Msg)
init _ = ( allOffButOne ((size//4), (size//4), 1) size p
          ,Cmd.none
          )

randomSeq: Random.Generator (List Int)
randomSeq  =
    Random.list (size//2) (Random.int 0 (p-1))

allOffButOne: Position -> Int -> Int -> Model
allOffButOne pos boardSize levelNum =
    let
        pos0 = List.concat <|
                        List.map (\x ->
                                      List.concat <| 
                                      List.map (\y ->
                                                    if x+y < boardSize then
                                                        [(x, y, 0)]
                                                    else
                                                        []
                                               )
                                      (List.range 0 (boardSize-1))
                                 )
                       (List.range 0 (boardSize-1))
        pos1 = List.concat <|
                  List.map (\x ->
                                List.concat <|
                                List.map (\y ->
                                              if x+y+1 < boardSize then
                                                  [(x, y, 1)]
                                              else
                                                  []
                                         )
                                (List.range 0 (boardSize-1))
                           )
                      (List.range 0 (boardSize-1))
        positions = pos0++pos1
        lights= List.foldl (\pt dict -> if pt == pos then
                                            Dict.insert pt 1 dict
                                        else
                                            Dict.insert pt 0 dict
                           ) Dict.empty  positions
    in
        Model lights [] boardSize levelNum
    
        
initialize: List Int -> Int -> Int -> Model
initialize halfSeq boardSize levelNum =
    let
        undetermined0 = List.concat <|
                        List.map (\x ->
                                      List.concat <| 
                                      List.map (\y ->
                                                    if x+y < boardSize then
                                                        [(x, y, 0)]
                                                    else
                                                        []
                                               )
                                      (List.range 0 (boardSize-1))
                                 )
                       (List.range 1 (boardSize-1))
        undetermined1 = List.concat <|
                        List.map (\x ->
                                      List.concat <|
                                      List.map (\y ->
                                                    if x+y+1 < boardSize then
                                                        [(x, y, 1)]
                                                    else
                                                        []
                                               )
                                      (List.range 0 (boardSize-1))
                                 )
                       (List.range 0 (boardSize-1))
        undetermined = undetermined0++undetermined1
        fullSeq =
            let
                evenSum = List.sum <| List.indexedMap (\i a -> if (modBy 2 i) == 0 && i>0 then
                                                                   a
                                                               else
                                                                   0
                                                      ) halfSeq
                head = modBy levelNum <| boardSize*levelNum - evenSum
                newHalfSeq = head::(List.drop 1 halfSeq)
            in
                List.concat [newHalfSeq
                              ,if (modBy 2 boardSize) == 0 then
                                   []
                               else
                                   [0]
                              ,List.reverse <| List.indexedMap (\i a -> modBy levelNum (4*levelNum+(-1)^i*a)) newHalfSeq
                              ]
        determined = List.foldl (\r dict -> Dict.insert (0, Dict.size dict, 0) r dict
                                 ) Dict.empty  fullSeq
        dummy = Debug.log "" <| undetermined
        dummy2 = Debug.log "" <| determined
    in
        Model determined undetermined boardSize levelNum
            
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Determine ->  (determine model, Cmd.none)
        Complete -> (complete model, Cmd.none)
        Reset -> (allOffButOne ((size//4), (size//4), 1) size p, Cmd.none)
        RandGenerated rseq  ->
            (initialize (Debug.log "random" <| rseq) size p, Cmd.none)
        Clicked (x,y,c) ->
            (rotation (x,y,c) model, Cmd.none)
            

view: Model -> Html Msg
view model =
    Html.div []
        [Html.button
             [Html.Events.onClick Reset]
             [Html.text "reset"]             
        ,Html.br [][]
        ,svg [width "800"
             ,height "800"
             ,viewBox "-100 -200 700 600"
             ]
             [boardView model]
        ]

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none
