module Main (..) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Element
import Graphics.Collage as Collage exposing (circle, filled, move, alpha)
import Keyboard
import Time exposing (Time)
import Text
import Color exposing (Color)
import Particle exposing (Particle, ParticleSystem)
import Names


sprayDirection =
  turns (3 / 8)


type alias Model =
  { clock : Time
  , particles : List Particle
  , system : ParticleSystem
  , name : String
  , seed : Seed
  }


init : Seed -> Model
init seed =
  let
    gen =
      Random.map2 (Model 0 []) Particle.setup Names.generate

    ( seedToModel, seed1 ) =
      Random.generate gen seed
  in
    seedToModel seed1


type Action
  = Tick Time
  | Regen


actions =
  let
    ticks =
      Signal.map Tick <| Time.fps 20

    regen =
      Signal.filterMap
        (\b ->
          if b then
            Just Regen
          else
            Nothing
        )
        Regen
        Keyboard.space
  in
    Signal.merge ticks regen


update : Action -> Model -> Model
update action model =
  case action of
    Regen ->
      init model.seed

    Tick dt ->
      { model
        | clock = model.clock + dt
        , particles =
            List.map (Particle.update dt) model.particles
              |> List.filter (\p -> Particle.alpha p /= 0)
      }
        |> maybeSpawnParticle


maybeSpawnParticle : Model -> Model
maybeSpawnParticle model =
  let
    maybeCons mx xs =
      case mx of
        Just x ->
          x :: xs

        Nothing ->
          xs

    smallProb =
      Random.map (\i -> i == 0) <| Random.int 0 4

    maybeParticle =
      smallProb
        `Random.andThen` (\b ->
                            if b then
                              Random.map Just <| Particle.init model.system
                            else
                              Random.constant Nothing
                         )

    ( mpart, seed1 ) =
      Random.generate maybeParticle model.seed
  in
    { model | particles = maybeCons mpart model.particles, seed = seed1 }


model : Signal Model
model =
  let
    seed =
      Random.initialSeed2 734080189 3044306560
  in
    Signal.foldp update (init seed) actions


view model =
  let
    s =
      400

    fmtText =
      -- TODO pick typeface at random
      Text.fromString >> Text.color Color.white >> Text.height 20 >> Text.typeface [ "Copperplate", "Luminari", "Herculanum" ] >> Collage.text
  in
    Collage.collage
      s
      s
      [ Collage.square (toFloat s) |> filled Color.black
      , List.map Particle.view model.particles |> Collage.group
      , fmtText model.name |> Collage.moveY (toFloat <| -s // 2 + 30)
      ]


main =
  Signal.map view model
