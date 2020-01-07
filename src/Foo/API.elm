module Foo.API exposing (..)


type alias Acknowledgement x =
    Result x ()


type alias Response x =
    Result x String


type alias API x a b =
    { listThings : ListThingInput -> List a
    , thing : ShowThingInput -> Maybe b
    , createThing : CreateThingInput -> Result x Thing
    , updateThing : UpdateThingInput -> Result x Thing

    -- bug with `, deleteThing : DeleteThingInput -> Result x ()`
    , deleteThing : DeleteThingInput -> Acknowledgement x
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
