module Weapon (Weapon, init, length, view) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Collage as Collage exposing (rect, ngon, circle, filled, move)
import Color exposing (Color)
import Particle exposing (ParticleSystem)


type Weapon
  = Wand WandInfo


type alias WandInfo =
  { primary : Color, accent : Color, length : Float, width : Float }


genBrown : Generator Color
genBrown =
  Random.map2
    (Color.hsl <| degrees 40)
    (Random.float 0.57 1)
    (Random.float 0.12 0.25)
    |> Random.map (Debug.log "Brown")


init : ParticleSystem -> Generator Weapon
init system =
  Random.map4 WandInfo genBrown genBrown (Random.float 150 280) (Random.float 2 12)
    |> Random.map Wand


length : Weapon -> Float
length weapon =
  case weapon of
    Wand { length } ->
      length


view : Weapon -> Collage.Form
view weapon =
  case weapon of
    Wand info ->
      drawWand info


drawWand : WandInfo -> Collage.Form
drawWand { primary, accent, length, width } =
  let
    handleLength =
      0.3 * length
  in
    Collage.group
      [ rect length width |> filled primary
      , rect handleLength (1.5 * width) |> filled accent |> Collage.moveX (-0.35 * length)
      , circle (width / 2.3) |> filled primary |> Collage.moveX (length / 2)
      ]
