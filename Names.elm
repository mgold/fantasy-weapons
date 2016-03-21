module Names (generate, typeface) where

import Random.PCG as Random exposing (Seed, Generator)
import String


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
      pickStrings
        [ ""
        , ""
        , ""
        , ""
        , ""
        , "Legendary "
        , "Fabled "
        , "Ancient "
        , "Cursed "
        , "Sacred "
        , "Enchanted "
        , "One "
        , "Second "
        ]
        []

    weapon =
      pickStrings
        [ "Wand"
        , "Staff"
        , "Rod"
        , "Source"
        , "Scepter"
        , "Mace"
        , "Caduceus"
        , "Stave"
        ]
        []

    topic =
      pickStrings
        [ "Healing"
        , "Fire"
        , "Levitation"
        , "Mind Control"
        , "Power"
        , "Light"
        , "Darkness"
        , "Flight"
        , "Invisibility"
        , "Electricity"
        , "Misfortune"
        , "Knowledge"
        , "Longevity"
        , "Summoning"
        , "Desire"
        , "Order"
        , "Chaos"
        , "Balance"
        ]
        []
  in
    Random.map3 (\a b c -> "The " ++ a ++ b ++ " of " ++ c) adj weapon topic


genName2 : Generator String
genName2 =
  let
    element =
      pickStrings [ "Earth", "Water", "Fire", "Air" ] []

    person =
      pickStrings
        [ "Err-mor"
        , "Wyvirnn"
        , "Vaelgoff"
        , "Aetravir"
        , "Ormin-Shad"
        , "Agamemnon"
        , "Saturn"
        , "Zeus"
        , "Atlas"
        , "Athena"
        , "Hera"
        ]
        [ Random.map (\elem -> elem ++ " Elemental") element
        ]

    situation =
      pickStrings
        [ "Folly"
        , "Last Resort"
        , "Woe"
        , "Terror"
        , "Bane"
        , "Gamble"
        , "Abomination"
        , "Regret"
        , "Mistake"
        , "Weapon of Choice"
        , "Invention"
        , "Delight"
        , "Secret"
        ]
        []
  in
    Random.map2 (\a b -> a ++ "'s " ++ b) person situation


generate : String -> Generator String
generate lastVal =
  let
    forbiddenWords =
      String.words lastVal
        |> List.filter (\w -> w /= "The" && w /= "of")
  in
    pickStrings [] [ genName1, genName2 ]
      |> Random.filter
          (\s ->
            String.length s
              < 37
              && not (List.any (\forbidden -> String.contains forbidden s) forbiddenWords)
          )


typeface : Generator (List String)
typeface =
  let
    fonts =
      [ "Papyrus", "Copperplate", "Luminari", "Herculanum" ]
  in
    Random.int 1 (List.length fonts)
      |> Random.map
          (\i ->
            List.drop i fonts ++ List.take i fonts
          )
