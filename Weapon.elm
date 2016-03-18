module Weapon (Weapon, init, length, particleOrigin, view) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Collage as Collage exposing (rect, ngon, circle, filled, move)
import Color exposing (Color)
import Time exposing (Time)
import Transform2D
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


{-| At a given time, provides the origin of particles and which direction they should go.
-}
particleOrigin : Time -> Weapon -> ( ( Float, Float ), Float )
particleOrigin t w =
  let
    r =
      length w * 0.9

    theta =
      wandAngle t

    ( x, y ) =
      fromPolar ( r, theta )
  in
    ( ( 20 + length w / 2.4 + x, y - 100 ), theta )


view : Time -> Weapon -> Collage.Form
view t weapon =
  case weapon of
    Wand info ->
      drawWand t info


drawWand : Time -> WandInfo -> Collage.Form
drawWand t { primary, accent, length, width } =
  let
    handleLength =
      0.3 * length
  in
    [ rect length width |> filled primary
    , rect handleLength (1.5 * width) |> filled accent |> Collage.moveX (-0.35 * length)
    , circle (width / 2.3) |> filled primary |> Collage.moveX (length / 2)
    ]
      |> rotateAroundX
          (length / 2.4)
          (wandAngle t)


thetaMin =
  turns 0.55


thetaMax =
  turns 0.24


wandAngle t =
  flickBetween thetaMin thetaMax (t / 3000)


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


rotateAroundX centerOfRotation angle forms =
  let
    moveOut =
      Transform2D.translation centerOfRotation 0

    moveDown =
      Transform2D.translation 20 -100

    rot =
      Transform2D.rotation angle

    tform =
      moveOut
        `Transform2D.multiply` moveDown
        `Transform2D.multiply` rot
        `Transform2D.multiply` moveOut
  in
    Collage.groupTransform tform forms
