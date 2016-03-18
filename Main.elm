module Main (..) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Element
import Graphics.Collage as Collage exposing (circle, rect, filled, move, rotate, alpha)
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
  , typeface : String
  , seed : Seed
  }


init : Seed -> Model
init seed =
  let
    baseModel =
      Model 0 []

    gen =
      Random.map3 baseModel Particle.setup Names.generate Names.typeface

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

    -- TODO move to system
    smallProb =
      Random.oneIn 5

    maybeParticle =
      Random.maybe smallProb (Particle.init model.system)

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
      Text.fromString
        >> Text.color Color.white
        >> Text.height 20
        >> Text.typeface [ model.typeface ]
        >> Collage.text

    wandLength =
      200

    staff =
      rect wandLength 10 |> filled Color.brown

    particles =
      List.map Particle.view model.particles
        |> Collage.group
        |> Collage.moveX (wandLength / 2)
  in
    Collage.collage
      s
      s
      [ Collage.square (toFloat s) |> filled Color.black
      , Collage.group [ staff, particles ]
          |> rotate (turns (3 / 8))
          |> move ( 0.2 * wandLength, -0.2 * wandLength )
      , fmtText model.name |> Collage.moveY (toFloat <| -s // 2 + 30)
      ]


main =
  Signal.map view model
