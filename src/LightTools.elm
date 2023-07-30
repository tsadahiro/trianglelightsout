module LightTools exposing (..)

import Types exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import TypedSvg.Color as Color
import Dict exposing (Dict)

adjacent01: Position -> Position -> Bool
adjacent01 (x0,y0,c0) (x1,y1,c1) =
    if c0 == c1 then
        False
    else
        if x0 == x1 && y0 == y1 then
            True
        else
            if x0 == x1+1 && y0 == y1 && c0 == 0 then
                True
            else
                if x0 == x1 && y0 == y1+1 && c0 == 0 then
                    True
                else
                    False

adjacent: Position -> Position -> Bool
adjacent l0 l1 =
    adjacent01 l0 l1 || adjacent01 l1 l0

possibleNeighbors: Position -> List Position
possibleNeighbors (x,y,c) = 
    if c==0 then
        [(x,y,1),(x-1,y,1),(x,y-1,1)]
    else
        [(x,y,0),(x+1,y,0),(x,y+1,0)]
        
determine: Model -> Model
determine model = 
    let
        boundary = Debug.log "boundary" <| List.filter
                   (\pos -> (List.length <| List.filter (\u -> adjacent pos u) model.undetermined ) == 1)
                   <| Dict.keys model.determined
        neighborSum p =
            Dict.foldl (\q value sum -> if adjacent p q then
                                            sum+value
                                        else
                                            sum
                       ) (Maybe.withDefault 0 <| Dict.get p model.determined )  model.determined
        newDetermined = List.foldl
                        (\p dict ->
                             List.foldl (\q dict2 ->
                                             if List.member q model.undetermined then
                                                 Dict.insert q (modBy model.p (4*model.p-(neighborSum p))) dict2
                                             else
                                                 dict2
                                        )
                             dict (possibleNeighbors p)
                        )
                        Dict.empty  boundary
        newUndetermined = List.filter (\p  -> not <| List.member p <| Dict.keys newDetermined)
                          model.undetermined 
    in
        {model | determined = Dict.union model.determined newDetermined
        ,undetermined = newUndetermined
        }

rotation: Position -> Model -> Model
rotation pos model =
    {model | determined =  Dict.map (\pt state -> if pt == pos || adjacent pt pos then
                                                      modBy model.p (state+1)
                                                  else
                                                      state
                                    ) model.determined
    }
        
complete: Model -> Model
complete model =
    if (List.length model.undetermined) > 0 then
        complete <| determine model
    else
        model
        
        
lightView: Position -> Int -> Int -> Int -> Svg Msg
lightView (x,y,c) brightness p size =
    let
        unit = 1000/(toFloat size)/3.5
        px = (toFloat (x+y))*1.5*unit
        py = (toFloat (y-x))*0.866*unit
    in
    if c == 0 then
        g [transform <| "translate ("++(String.fromFloat px)++", "++(String.fromFloat py)++")"
          ,onClick (Clicked (x,y,c))
          ]
            [Svg.path [d ("M -"++(String.fromFloat unit)
                          ++" 0 l "
                          ++(String.fromFloat (1.5*unit))
                          ++" "
                          ++(String.fromFloat ((sqrt 3)/2.0*unit))
                          ++" l 0 "
                          ++(String.fromFloat -((sqrt 3)*unit))
                          ++" z"
                     )
                  ,fill <| Color.colorToCssRgba <| Color.rgb (255//(p)*(p-brightness)) (255//(p)*(p-brightness)) 255
                  ,stroke "gray"
                  ][]
            ]
    else
        g [transform <| "translate ("++(String.fromFloat (px+(unit)))++", "++(String.fromFloat py++")")
          ,onClick (Clicked (x,y,c))
          ]
            [Svg.path [d ("M "++(String.fromFloat unit)
                          ++" 0 l "
                          ++(String.fromFloat (-1.5*unit))
                          ++" "
                          ++(String.fromFloat ((sqrt 3)/2.0*unit))
                          ++" l 0 "
                          ++(String.fromFloat -((sqrt 3)*unit))
                          ++" z"
                     )
                  ,fill <| Color.colorToCssRgba <| Color.rgb (255//(p)*(p-brightness)) (255//(p)*(p-brightness)) 255
                  ,stroke "gray"
                  ][]
             ]

slotView: Position -> Svg Msg
slotView (x,y,c) =
    let
        unit = 100
        px = (toFloat (x+y))*1.5*unit
        py = (toFloat (y-x))*0.866*unit
    in
    if c == 0 then
        g [transform <| "translate ("++(String.fromFloat px)++", "++(String.fromFloat py)++")"]
            [Svg.path [d ("M -"++(String.fromFloat unit)
                          ++" 0 l "
                          ++(String.fromFloat (1.5*unit))
                          ++" "
                          ++(String.fromFloat ((sqrt 3)/2.0*unit))
                          ++" l 0 "
                          ++(String.fromFloat -((sqrt 3)*unit))
                          ++" z"
                     )
                  ,fill "blue"
                  ,stroke "gray"
                  ][]
            ]
    else
        g [transform <| "translate ("++(String.fromFloat (px+(toFloat unit)))++", "++(String.fromFloat py++")")]
            [Svg.path [d ("M "++(String.fromFloat unit)
                          ++" 0 l "
                          ++(String.fromFloat (-1.5*unit))
                          ++" "
                          ++(String.fromFloat ((sqrt 3)/2.0*unit))
                          ++" l 0 "
                          ++(String.fromFloat -((sqrt 3)*unit))
                          ++" z"
                     )
                  ,fill "red"
                  ,stroke "gray"
                  ][]
             ]
                
        
boardView: Model -> Svg Msg
boardView model =
    g [] <|
        Dict.foldl (\pos brightness list ->
                        (lightView pos brightness model.p model.size)::list) [] model.determined
        
