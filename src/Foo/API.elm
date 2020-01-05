module Foo.API exposing (..)


type alias Acknowledgement x =
    Result x ()


type alias Response x =
    Result x String


type alias API x =
    { listThings : ListThingInput -> Result x (List Thing)
    , thing : ShowThingInput -> Result x Thing
    , createThing : CreateThingInput -> Result x Thing
    , updateThing : UpdateThingInput -> Result x Thing
    , deleteThing : DeleteThingInput -> Result x ()
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