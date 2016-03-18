module Weapon (Weapon, init, length, particleOrigin, view) where

import Random.PCG as Random exposing (Seed, Generator)
import Graphics.Collage as Collage exposing (rect, ngon, circle, filled, move)
import Color exposing (Color)
import Time exposing (Time)
import Transform2D
import Particle exposing (ParticleSystem)


type Weapon
  = Wand WandInfo


type Embellishment
  = Plain
  | EndGrips
  | Pommel Color
  | HiltGems Color
  | BladeLine Color
  | HiltLine Color
  | Crossguard Color


type alias WandInfo =
  { primary : Color, accent : Color, length : Float, width : Float, embellishment : Embellishment }


genBrown : Generator Color
genBrown =
  Random.map2
    (Color.hsl <| degrees 40)
    (Random.float 0.57 1)
    (Random.float 0.12 0.25)


init : ParticleSystem -> Generator Weapon
init system =
  Random.map5 WandInfo genBrown genBrown (Random.float 150 280) (Random.float 2 12) (genEmbellishment system)
    |> Random.map verifyEmbellishment
    |> Random.map Wand


genEmbellishment : ParticleSystem -> Generator Embellishment
genEmbellishment { primary, accent } =
  let
    choose clr i =
      case i of
        1 ->
          EndGrips

        2 ->
          Pommel clr

        3 ->
          HiltGems clr

        4 ->
          BladeLine clr

        5 ->
          HiltLine clr

        6 ->
          Crossguard clr

        7 ->
          EndGrips

        _ ->
          Plain
  in
    Random.map2
      choose
      (Random.choice primary accent)
      (Random.int 0 9)


{-| Not all embellishments look good on thin wands.
-}
verifyEmbellishment : WandInfo -> WandInfo
verifyEmbellishment wand =
  if wand.width > 5 then
    wand
  else
    case wand.embellishment of
      Crossguard _ ->
        { wand | embellishment = Plain }

      BladeLine _ ->
        { wand | embellishment = EndGrips }

      _ ->
        wand


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
drawWand t ({ primary, accent, length, width } as wand) =
  let
    handleLength =
      0.3 * length
  in
    [ rect length width |> filled primary
    , rect handleLength (1.5 * width) |> filled accent |> Collage.moveX (-0.35 * length)
    , circle (width / 2.3) |> filled primary |> Collage.moveX (length / 2)
    , embellishmentForm wand
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


embellishmentForm : WandInfo -> Collage.Form
embellishmentForm wand =
  case wand.embellishment of
    EndGrips ->
      let
        grip =
          rect 5 (wand.width * 2.4) |> filled wand.accent
      in
        Collage.group
          [ grip |> Collage.moveX (-wand.length / 2)
          , grip |> Collage.moveX (-wand.length * 0.2)
          ]

    Pommel clr ->
      Collage.group
        [ circle (1.4 * wand.width) |> filled wand.accent
        , circle (0.7 * wand.width) |> filled clr
        ]
        |> Collage.moveX (-wand.length / 2)

    HiltGems clr ->
      let
        gem =
          circle (0.3 * wand.width) |> filled clr
      in
        List.map (\dist -> Collage.moveX (-wand.length * dist) gem) [ 0.45, 0.35, 0.25 ]
          |> Collage.group

    BladeLine clr ->
      rect (0.7 * wand.length) 1 |> filled clr |> move ( wand.length * 0.15, -2 )

    HiltLine clr ->
      rect (0.3 * wand.length) 1 |> filled clr |> move ( -wand.length * 0.35, 2 )

    Crossguard clr ->
      [ rect 6 (8 * wand.width |> min 50) |> filled Color.charcoal
      , ngon 3 10 |> filled Color.charcoal |> Collage.rotate pi |> Collage.moveX -3
      , circle 2.5 |> filled clr |> Collage.moveX -2
      ]
        |> Collage.group
        |> Collage.moveX (-wand.length * 0.2)

    _ ->
      Collage.group []
