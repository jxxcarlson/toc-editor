module HTOC exposing (Item, SimpleItem, increment,decrement, init, update, getLevel, getSectionNumber)

{-| This library facilitates editing of multi-level tables of contents and can be used,
    as in the demo, to construct a drag-and-drop editor for such tables.



## Types
@docs Item, SimpleItem

## API for drag-and-drop editor
@docs  init, update, increment, decrement

## Helpers for exporting computed information

These helpers are useful if a `List Item` must
be transformed to a `List SomeOtherKindOfItem`.

@docs getLevel, getSectionNumber

-}

import HTree 
import Tree exposing(Tree)

type alias Id = String  



{-| The type for tables of content.

-}
type alias Item =
  {levels : Levels, title: String, id: Id, sectionNumber : String }


{-| This is the type of items that the `init` function
operates on to produce a list of `Item`.  If the table
editor must operate on a list of items from "outside,"
those items must have the fields `level`, `title`, and `id`.

-}
type alias SimpleItem a =
  {a | level : Int, title: String, id: Id }

type alias Levels = { curr : Int, prev : Int}



{-|

 Update the levels and section numbers of each item.  These
 change if the items or reordered or their levels are changed.
 The update function repairs lists that are not well-formed.

-}
update : List Item -> List Item
update items = 
  items
    |> rectify
    |> updateLevels
    |> tagTree
    |> Tree.flatten  
    |> List.drop 1 
    |> List.map (\(item, sectionNumber) -> {item | sectionNumber = sectionNumber})



-- TREE OPERATIONS
{-|
  Return the updated level of an item.
-}
getLevel : Item -> Int
getLevel item = item.levels.curr

{-|
  Return the updated section number of an item.
-}
getSectionNumber : Item -> String
getSectionNumber item = item.sectionNumber

root : Item 
root = { levels = { curr = 0, prev = -1} , title = "root", id = "*", sectionNumber = "" } 

tagTree : List Item -> Tree (Item, String)
tagTree items = 
   items
      |> HTree.fromList root getLevel
      |> tagWithMultiIndex

tagWithMultiIndex : Tree a -> Tree ( a, String )
tagWithMultiIndex t =
    tagWithMultiIndexHelp [] t
      |> Tree.map (\(info, idx) -> (info, stringFromIndex idx))

stringFromIndex : List Int -> String 
stringFromIndex is = 
  is |> List.reverse |> List.map String.fromInt |> String.join "."

tagWithMultiIndexHelp : List Int -> Tree a -> Tree ( a, List Int )
tagWithMultiIndexHelp ks t =
    let
        c =
            Tree.children t
    in
    Tree.tree ( Tree.label t, ks ) (List.indexedMap (\i t_ -> tagWithMultiIndexHelp (i+1::ks) t_) c)  


-- Item <--> Item

toSimpleItem : Item -> SimpleItem {}
toSimpleItem i = {title = i.title, id = i.id, level = i.levels.curr }


{-|

Transform a list of SimpleItems to a list of items.  The essential
point is to compute a levels field, which is used internally to ensure
that the TOC is well-formed.

-}
init : List (SimpleItem a) -> List Item
init items =
    let
        levels = makeLevels (List.map .level items)  
    in
        List.map2 (\item level -> toItemWithLevel item level) items levels

toItemWithLevel : SimpleItem a -> Levels -> Item
toItemWithLevel item levels = 
  { title = item.title, id = item.id, sectionNumber = "", levels = levels}



-- LEVELS

updateLevels : List Item -> List Item
updateLevels items = 
    let
        ls = makeLevels (List.map getLevel items)
    in
        List.map2 (\item levels -> {item | levels = levels}) items ls


makeLevels : List Int -> List Levels
makeLevels js = 
  let
    n = List.length js
    is = List.take n ((-1)::js)
  in
    List.map2 (\i j -> {prev = i, curr = j}) is js


-- INCREMENT AND DECREMENT


{-|

Increment the level of the item in a list with given id.

-}
increment : Id -> List Item -> List Item    
increment  id items = 
   List.map (incrementItem_ id ) items


{-| 

Decrement the level of the item in a list with given id.

-}
decrement : Id -> List Item -> List Item    
decrement  id items = 
   List.map (decrementItem_ id ) items


incrementItem_ : Id -> Item -> Item 
incrementItem_ id item = 
  if id == item.id && canIncrement item then 
    { item | levels = incrementLevels item.levels }
  else 
    item


decrementItem_ : Id -> Item -> Item 
decrementItem_ id item = 
  if id == item.id && canDecrement item then 
    { item | levels = decrementLevels item.levels }
  else 
    item    

canIncrement : Item -> Bool
canIncrement item = item.levels.curr <= item.levels.prev + 1

incrementLevels : Levels -> Levels 
incrementLevels {curr, prev} = {curr = curr + 1, prev = prev}


canDecrement : Item -> Bool
canDecrement item = item.levels.curr > 0

decrementLevels : Levels -> Levels 
decrementLevels {curr, prev} = {curr = curr - 1, prev = prev}

setLevel_ : Int -> Levels -> Levels 
setLevel_ k levels =
   {curr = k, prev = levels.prev}

setLevel : Int -> Item -> Item
setLevel k item = 
  {item | levels = setLevel_ k item.levels}



-- RECTIFICATION

{-
  After rectification, a list of items is well-formed, meaning that the
  the level of an item is at most the level of the previous item plus one,
  and the level of the first item is zero.  If you think the items as
  definiing an outline, the indentation of an item is proporitiaonl to
  its level.

-}
rectify : List Item -> List Item
rectify items =
   case List.head items of
        Nothing -> []
        Just firstItem_ -> 
          let 
            firstItem = setLevel 0 firstItem_
            initialState = { input = List.drop 1 items, currentItem = firstItem, output = [firstItem] }
          in
            loop initialState nextMState
              |> List.reverse

type alias MState = { input : List Item, currentItem : Item, output : List Item}


nextMState :  MState -> Step MState (List Item)
nextMState state = 
   case List.head state.input of
    Nothing -> Done state.output 
    Just item -> 
      if item.levels.curr <= state.currentItem.levels.curr + 1 then 
         Loop { input = List.drop 1 state.input, currentItem = item, output = item :: state.output } 
      else 
         let 
           newItem = setLevel (state.currentItem.levels.curr + 1) item 
         in
           Loop { input = List.drop 1 state.input, currentItem = newItem, output = newItem :: state.output } 
 



type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b