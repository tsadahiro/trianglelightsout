modle Light exposing (..)

import Types exposing (..)

adjacent01: Light -> Light -> Bool
adjacent01 l0 l1 =
    if l0.c == l1.c then
        False
    else
        if l0.x == l1.x && l0.y == l1.y then
            True
        else
            if l0.x == l1.x+1 && l0.y == l1.y && l0.c == 0 then
                True
            else
                if l0.x == l1.x && l0.y+1 == l1.y && l0.c == 0 then
                    True
                else
                    False

adjacent: Light -> Light -> Bool
adjacent l0 l1 =
    adjacent01 l0 l1 || adjacent01 l1 l0
