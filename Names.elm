module Names (generate, typeface) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Element
import Keyboard


type alias Model =
  { name : String
  , seed : Seed
  }


pickOne : List (Generator String) -> Generator String
pickOne xs =
  let
    get ys i =
      case ys of
        z :: zs ->
          if i == 0 then
            z
          else
            get zs (i - 1)

        [] ->
          Random.constant ""
  in
    Random.andThen (Random.int 0 (List.length xs - 1)) (get xs)


pickStrings : List String -> List (Generator String) -> Generator String
pickStrings ss gs =
  pickOne (List.map Random.constant ss ++ gs)


genName1 : Generator String
genName1 =
  let
    adj =
      pickStrings [ "", "", "", "Legendary ", "Fabled ", "Cursed ", "Sacred ", "Enchanted " ] []

    weapon =
      pickStrings [ "Wand", "Staff", "Rod", "Source", "Scepter", "Mace", "Caduceus" ] []

    topic =
      pickStrings [ "Healing", "Fire", "Levitation", "Mind Control", "Power", "Electricity", "Misfortune" ] []
  in
    Random.map3 (\a b c -> "The " ++ a ++ b ++ " of " ++ c) adj weapon topic


genName2 : Generator String
genName2 =
  let
    person =
      pickStrings [ "Err-mor", "Wyvirnn", "Vaelgoff", "Aetravir", "Agamemnon", "Saturn", "Zeus", "Atlas" ] []

    situation =
      pickStrings [ "Folly", "Last Resort", "Woe", "Terror", "Bane", "Gamble", "Abomination", "Regret" ] []
  in
    Random.map2 (\a b -> a ++ "'s " ++ b) person situation


generate : Generator String
generate =
  pickStrings [] [ genName1, genName2 ]


typeface : Generator String
typeface =
  pickStrings [ "Copperplate", "Luminari", "Herculanum" ] []



{-
   init : Model
   init =
     let
       seed =
         Random.initialSeed2 734080189 3044306560
     in
       uncurry Model <| Random.generate genName seed


   type Action
     = Regen


   actions =
     Signal.filterMap
       (\b ->
         if b then
           Just Regen
         else
           Nothing
       )
       Regen
       Keyboard.space


   update : Action -> Model -> Model
   update _ { seed } =
     uncurry Model <| Random.generate genName seed


   model : Signal Model
   model =
     Signal.foldp update init actions


   view { name } =
     Graphics.Element.show name


   main =
     Signal.map view model
-}
