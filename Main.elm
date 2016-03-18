module Main (..) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Element
import Graphics.Collage as Collage exposing (circle, rect, filled, move, rotate, alpha)
import Keyboard
import Time exposing (Time)
import Text
import Color exposing (Color)
import Transform2D
import Particle exposing (Particle, ParticleSystem)
import Names
import Weapon exposing (Weapon)


sprayDirection =
  turns (3 / 8)


type alias Model =
  { clock : Time
  , particles : List Particle
  , system : ParticleSystem
  , weapon : Weapon
  , name : String
  , typeface : String
  , seed : Seed
  }


init : Seed -> Model
init seed =
  let
    baseModel ( system, weapon ) =
      Model 0 [] system weapon

    genSystemAndWeapon : Generator ( ParticleSystem, Weapon )
    genSystemAndWeapon =
      Particle.setup `Random.andThen` (\sys -> Weapon.init sys |> Random.map (\wep -> ( sys, wep )))

    gen =
      Random.map3 baseModel genSystemAndWeapon Names.generate Names.typeface

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
      Signal.map Tick <| Time.fps 30

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

    maybeParticle =
      Random.maybe model.system.spawnProb (Particle.init model.system)

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


flickBetween : Float -> Float -> Float -> Float
flickBetween a b x =
  let
    integer =
      floor x

    t =
      toFloat (integer % 1) + x - toFloat integer

    scale =
      if t < 0.88 then
        sin (2.1 * t)
      else
        1.33 * (sin (10 * t + 0.9) + 0.995)
  in
    a + (b - a) * scale


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

    particles =
      List.map Particle.view model.particles
        |> Collage.group
        |> Collage.moveX (Weapon.length model.weapon / 2)

    rotateAroundX centerOfRotation angle forms =
      let
        moveOut =
          Transform2D.translation centerOfRotation 0

        moveDown =
          Transform2D.translation 0 -80

        rot =
          Transform2D.rotation angle

        tform =
          moveOut
            `Transform2D.multiply` moveDown
            `Transform2D.multiply` rot
            `Transform2D.multiply` moveOut
      in
        Collage.groupTransform tform forms
  in
    Collage.collage
      s
      s
      [ Collage.square (toFloat s) |> filled Color.black
      , [ Weapon.view model.weapon, particles ]
          |> rotateAroundX
              (Weapon.length model.weapon / 2.4)
              (flickBetween (turns 0.55) (turns 0.24) (model.clock / 3000))
      , fmtText model.name |> Collage.moveY (toFloat <| -s // 2 + 30)
      ]


main =
  Signal.map view model
