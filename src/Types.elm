module Types exposing (..)

import Dict exposing (Dict)

type alias Model = {determined: Dict Position Int
                   ,undetermined: List Position
                   ,size: Int
                   ,p: Int
                   }
type alias Position = (Int, Int, Int)

type Msg = Determine
    | Complete
    | Reset
    | RandGenerated (List Int)

    

