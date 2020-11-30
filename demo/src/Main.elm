module Main exposing (Model, Msg, initialModel, main, subscriptions, update, view)

import Browser
import DnDList
import Element
import Element.Input
import Element.Font
import Element.Background
import Html
import Html.Attributes
import HTOC exposing(Item, SimpleItem)




-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL, DATA


type alias Model =
    { dnd : DnDList.Model
    , items : List Item
    }




data : List (SimpleItem {})
data =
    [  { level = 0, title = "Mount Yadaax-Vor", id = "a"  }
    ,  {  level = 0, title =  "The Kraken Roars", id = "b"  }
     , { level = 0, title = "The Magic Stone", id = "c" }
     , { level = 0, title = "A Quiet Pool", id = "d"  }
     , { level = 0, title = "Lost on the River", id = "e"  }
     , { level = 0, title = "Crossing the Desert", id = "f" }
     , { level = 0, title = "A Strange Beast", id = "g"  }

    ]
        

-- SYSTEM


config : DnDList.Config Item
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system : DnDList.System Item Msg
system =
    DnDList.create config MyMsg


-- MODEL


initialModel : Model
initialModel =
    { dnd = system.model
    , items = HTOC.update (HTOC.init data)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd



-- UPDATE


type Msg
    = MyMsg DnDList.Msg
      | IncrementLevel String 
      | DecrementLevel String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MyMsg msg ->
            let
                ( dnd, items ) =
                    system.update msg model.dnd model.items
            in
            ( { model | dnd = dnd, items = HTOC.update items }
            , system.commands dnd
            )

        IncrementLevel id -> 
          let 
             items = HTOC.update (HTOC.increment id model.items)
          in
          ({ model | items = items }, Cmd.none)

        DecrementLevel id -> 
           let 
             items = HTOC.update (HTOC.decrement id model.items)
          in
          ({ model | items = items }, Cmd.none)




-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.section []
        [ Element.layoutWith { options = [ Element.focusStyle noFocus ]}
            [ Element.width  Element.fill
            , Element.height (Element.px 600)
            , Element.inFront (ghostView model.dnd model.items)
            , Element.centerY
            ]
            (Element.column
                [ Element.centerX, Element.centerY, Element.padding 10, Element.spacing 10 ]
                (Element.el [Element.Font.bold] (Element.text "Table of Contents") :: (model.items |>  List.indexedMap (itemView model.dnd))
            ))
        ]


itemView : DnDList.Model -> Int -> Item -> Element.Element Msg
itemView dnd index item =
    let
        itemId : String
        itemId =
            "id-" ++ item.title
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                (Element.row [Element.spacing 1] [
                    control item
                    , Element.el
                        (Element.Font.color (Element.rgb 0 0 0)
                            :: (offset item.levels.curr)
                            :: Element.htmlAttribute (Html.Attributes.id itemId)
                            :: List.map Element.htmlAttribute (system.dropEvents index itemId)
                        )
                       (Element.text (item.sectionNumber ++ " " ++item.title))
                ])

            else
                Element.el
                    [ Element.Font.color (Element.rgb 0 0 0)
                    , Element.htmlAttribute (Html.Attributes.id itemId)
                    ]
                    (Element.text "[---------]")

        Nothing ->
            (Element.row [Element.spacing 1] [
                control item
                , Element.el
                    (Element.Font.color (Element.rgb 0 0 0)
                        :: (offset item.levels.curr)
                        :: Element.htmlAttribute (Html.Attributes.id itemId)
                        :: List.map Element.htmlAttribute (system.dragEvents index itemId)
                    )
                    (Element.text (item.sectionNumber ++ " " ++item.title))
            ])


ghostView : DnDList.Model -> List Item -> Element.Element Msg
ghostView dnd items =
    let
        maybeDragItem : Maybe Item
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            Element.el
                (Element.Font.color (Element.rgba 0.7 0 0 0.4)
                    :: (offset item.levels.curr)
                    :: List.map Element.htmlAttribute (system.ghostStyles dnd)
                )
                (Element.text (item.sectionNumber ++ " " ++ item.title))

        Nothing ->
            Element.none  


-- BUTTONS & HELPERS


control item = Element.el [Element.paddingEach {left = 0, right = 12, top = 0, bottom = 0}]
      (Element.row [Element.paddingEach {left = 4, right = 4, top = 4, bottom = 2}, 
                lightBlueBackground, Element.spacing 4] [minusButton item.id, plusButton item.id])
plusButton id = 
  Element.Input.button ((Element.paddingXY 0 0)::buttonStyle) { onPress = Just (IncrementLevel id), label = Element.el [] (Element.text "▶")}

minusButton id = 
  Element.Input.button ((Element.paddingXY 0 0)::buttonStyle) { onPress = Just (DecrementLevel id), label = Element.el [] (Element.text "◀")}
          
offset k = 
  Element.paddingEach {left = 16*k, right = 0, top = 0, bottom = 0}

-- STYLE

noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }

background g = Element.Background.color (Element.rgb g g g)

darkBackground = background 0.4

lightBlueBackground = Element.Background.color (Element.rgb 0.9 0.9 1)

fontColor g = Element.Font.color (Element.rgb g g g)

fontColorWhite = fontColor 0.9

fontColorBlue =  Element.Font.color (Element.rgb 0.5 0.5 1)

buttonStyle =
 [  fontColorBlue
-- , Element.width (Element.px 18)
 --, Element.height (Element.px 18)
 , Element.Font.size 12
 ]
