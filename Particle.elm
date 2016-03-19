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
  , growth : Float
  , pos : ( Float, Float )
  , vel : ( Float, Float )
  , color : Color
  }


{-| The parameters of the system. Does not contain the particles, current time, etc.
-}
type alias ParticleSystem =
  { spreadAngle : Float
  , maxRadius : Generator Float
  , spawnProb : Generator Bool
  , growth : Float
  , primary : Color
  , accent : Color
  }


setup : Generator ParticleSystem
setup =
  let
    partial =
      Random.map4
        ParticleSystem
        --spreadAngle
        (Random.float 0 (turns 0.1))
        --maxRadius generator
        (Random.map2 (\a b -> Random.float a (a + b)) (Random.float 5 35) (Random.float 10 50))
        --spawnProb generator
        (Random.map Random.oneIn <| Random.int 1 7)
        --growth factor
        (Random.oneIn 10
          `Random.andThen` \b ->
                            if b then
                              Random.choice 1.5 1.001
                            else
                              Random.float 1.01 1.3
        )
  in
    Random.map3
      (\f hue dhue ->
        f (Color.hsl hue 0.9 0.5) (Color.hsl (hue + dhue) 1 0.6)
      )
      partial
      (Random.float 0 (turns 1))
      (Random.choice (degrees -30) (degrees 30))


init : Float -> ParticleSystem -> Generator Particle
init sprayAngle { maxRadius, spreadAngle, growth, primary, accent } =
  let
    particle maxR clr polarVel =
      Particle 1 maxR growth ( 0, 0 ) (fromPolar polarVel) clr

    angle =
      Random.float (sprayAngle - spreadAngle) (sprayAngle + spreadAngle)

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
    { p | pos = ( x + dx * dt, y + dy * dt ), radius = p.radius * p.growth }


alpha : Particle -> Float
alpha { radius, maxRadius } =
  1 - (min 1 (radius / maxRadius))


view : Particle -> Collage.Form
view ({ color, radius, pos } as p) =
  circle radius |> filled color |> move pos |> Collage.alpha (alpha p)
