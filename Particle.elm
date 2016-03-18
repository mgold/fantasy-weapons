module Particle (..) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Element
import Graphics.Collage as Collage exposing (circle, filled, move)
import Keyboard
import Time exposing (Time)
import Color exposing (Color)


type alias Particle =
  { radius : Float
  , maxRadius : Float
  , pos : ( Float, Float )
  , vel : ( Float, Float )
  , color : Color
  }


{-| The parameters of the system. Does not contain the particles, current time, etc.
-}
type alias ParticleSystem =
  { spreadAngle : Float
  , maxRadius : Generator Float
  , primary : Color
  , accent : Color
  }


setup : Generator ParticleSystem
setup =
  let
    system spread maxR hue dhue =
      ParticleSystem spread maxR (Color.hsl hue 0.9 0.5) (Color.hsl (hue + dhue) 1 0.6)
  in
    Random.map4
      system
      (Random.float 0 (turns 0.1))
      (Random.map2 (\a b -> Random.float a (a + b)) (Random.float 5 30) (Random.float 10 40))
      (Random.float 0 (turns 1))
      (Random.choice (degrees -30) (degrees 30))


init : ParticleSystem -> Generator Particle
init { maxRadius, spreadAngle, primary, accent } =
  let
    particle maxR clr polarVel =
      Particle 1 maxR ( 0, 0 ) (fromPolar polarVel) clr

    angle =
      Random.float -spreadAngle spreadAngle

    rad =
      Random.float 0.02 0.1

    clr =
      Random.choice primary accent
  in
    Random.map3 particle maxRadius clr <| Random.pair rad angle


update : Time -> Particle -> Particle
update dt p =
  let
    ( dx, dy ) =
      p.vel

    ( x, y ) =
      p.pos
  in
    { p | pos = ( x + dx * dt, y + dy * dt ), radius = p.radius * 1.1 }


alpha : Particle -> Float
alpha { radius, maxRadius } =
  1 - (min 1 (radius / maxRadius))


view : Particle -> Collage.Form
view ({ color, radius, pos } as p) =
  circle radius |> filled color |> move pos |> Collage.alpha (alpha p)
