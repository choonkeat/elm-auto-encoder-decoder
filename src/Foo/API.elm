module Foo.API exposing (..)


type alias Acknowledgement x =
    Result x ()


type alias Response x =
    Result x String


type alias API =
    { listThings : ListThingInput -> List Thing
    , thing : ShowThingInput -> Maybe Thing
    , createThing : CreateThingInput -> Result String Thing
    , updateThing : UpdateThingInput -> Result String Thing

    -- bug with `, deleteThing : DeleteThingInput -> Result x ()`
    , deleteThing : DeleteThingInput -> Acknowledgement String
    }


type ID
    = ID String


type alias Thing =
    { id : ID
    , name : String
    }


type alias ListThingInput =
    { since : Maybe ID
    , pageSize : Maybe Int
    }


type alias ShowThingInput =
    { id : ID
    }


type alias CreateThingInput =
    { name : String
    }


type alias UpdateThingInput =
    { id : ID
    , name : String
    }


type alias DeleteThingInput =
    { id : ID
    }
