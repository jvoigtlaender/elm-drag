module Main (main) where

import Dict
import Drag exposing (..)
import Graphics.Input
import Text exposing (fromString)
import Graphics.Element exposing (color, layers, leftAligned, sizeOf)
import Graphics.Collage exposing (collage, outlined, rect, solid, toForm)
import Color exposing (black, orange, yellow)
import Html exposing (button, div, fromElement, text)
import Html.Events exposing (onClick)
import StartApp
import Effects


noEffects =
    flip (,) Effects.none


hover =
    Signal.mailbox Nothing


makeBox i =
    Graphics.Input.hoverable
        (Signal.message hover.address
            << \h ->
                if h then
                    Just i
                else
                    Nothing
        )
        (putInBox (leftAligned (fromString (toString i))))


putInBox e =
    let
        ( sx, sy ) = sizeOf e
    in
        layers [ e, collage sx sy [ outlined (solid black) (rect (toFloat sx) (toFloat sy)) ] ]


moveBy ( dx, dy ) ( x, y ) =
    ( x + toFloat dx, y - toFloat dy )


model =
    { i = 0, dict = Dict.empty }


type Action
    = Add
    | Track (Maybe ( Int, Drag.Action ))


update action model =
    noEffects
        <| case action of
            Add ->
                let
                    i = model.i + 1
                in
                    { i = i, dict = Dict.insert i ( ( 0, 0 ), color yellow (makeBox i) ) model.dict }

            Track (Just ( i, Lift )) ->
                { model | dict = Dict.update i (Maybe.map (\( p, b ) -> ( p, color orange b ))) model.dict }

            Track (Just ( i, MoveBy ( dx, dy ) )) ->
                { model | dict = Dict.update i (Maybe.map (\( p, b ) -> ( moveBy ( dx, dy ) p, b ))) model.dict }

            Track (Just ( i, Release )) ->
                { model | dict = Dict.update i (Maybe.map (\( p, b ) -> ( p, color yellow b ))) model.dict }

            _ ->
                model


view address { dict } =
    div
        []
        [ button [ onClick address Add ] [ text "add a draggable box" ]
        , fromElement (collage 200 200 (List.map (\( p, b ) -> Graphics.Collage.move p (toForm b)) (Dict.values dict)))
        ]


app =
    StartApp.start
        { init = noEffects model
        , update = update
        , view = view
        , inputs = [ Signal.map Track (trackMany Nothing hover.signal) ]
        }


main =
    app.html
