module Particle (..) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Element
import Graphics.Collage as Collage exposing (circle, filled, move)
import Keyboard
import Time exposing (Time)
import Color exposing (Color)


type alias Particle =
  { radius : Float
  , pos : ( Float, Float )
  , vel : ( Float, Float )
  , color : Color
  }


{-| The parameters of the system. Does not contain the particles, current time, etc.
-}
type alias ParticleSystem =
  { sprayAngle : Float
  , spreadAngle : Float
  , primary : Color
  , accent : Color
  }


setup : Generator ParticleSystem
setup =
  let
    system spread hue dhue =
      ParticleSystem (turns 0.25) spread (Color.hsl hue 0.9 0.5) (Color.hsl (hue + dhue) 1 0.6)
  in
    Random.map3
      system
      (Random.float 0 (turns (1 / 12)))
      (Random.float 0 (turns 1))
      (Random.choice (degrees -30) (degrees 30))


init : ParticleSystem -> Generator Particle
init { sprayAngle, spreadAngle, primary, accent } =
  let
    particle clr polarVel =
      Particle 1 ( 0, 0 ) (fromPolar polarVel) clr

    angle =
      Random.float (sprayAngle - spreadAngle) (sprayAngle + spreadAngle)

    rad =
      Random.float 0.1 0.5

    clr =
      Random.choice primary accent
  in
    Random.map2 particle clr <| Random.pair rad angle


update : Time -> Particle -> Particle
update dt p =
  let
    ( dx, dy ) =
      p.vel

    ( x, y ) =
      p.pos
  in
    { p | pos = ( x + dx * dt, y + dx * dt ), radius = p.radius * 1.1 }


alpha : Particle -> Float
alpha { radius } =
  1 - (min 1 (radius / 40))


view : Particle -> Collage.Form
view ({ color, radius, pos } as p) =
  circle radius |> filled color |> move pos |> Collage.alpha (alpha p)
