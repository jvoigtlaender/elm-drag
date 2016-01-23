elm-drag
========

A low- and high-level interface to mouse drag actions in
[Elm](http://elm-lang.org/). For documentation of the latest
published version, see also
http://package.elm-lang.org/packages/jvoigtlaender/elm-drag/latest/Drag.

For reporting any issues, see
https://github.com/jvoigtlaender/elm-drag/issues.

The low-level interface is:

```elm
type MouseEvent
    = StartAt ( Int, Int )
    | MoveFromTo ( Int, Int ) ( Int, Int )
    | EndAt ( Int, Int )

mouseEvents : Signal MouseEvent
```

The recommended, high-level interface consists of the
`track`-functions:

```elm
type Action
    = Lift
    | MoveBy ( Int, Int )
    | Release

track : Bool -> Signal Bool -> Signal (Maybe Action)

trackMany : Maybe a -> Signal (Maybe a) -> Signal (Maybe ( a, Action ))
```

In those `track`-functions, the `Bool`/`Signal Bool` or `Maybe
a`/`Signal (Maybe a)` arguments are the initial value and input signal
which tell whether the mouse is (currently) hovering over something
draggable. See
[Example1.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example1.elm)
([demo](https://jvoigtlaender.github.io/elm-drag/Example1.html)),
[Example2.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example2.elm)
([demo](https://jvoigtlaender.github.io/elm-drag/Example2.html)),
[Example3.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example3.elm)
([demo](https://jvoigtlaender.github.io/elm-drag/Example3.html)),
and
[Example3b.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example3b.elm)
([demo](https://jvoigtlaender.github.io/elm-drag/Example3b.html)).

The library also exposes an
[Automaton](http://package.elm-lang.org/packages/evancz/automaton/latest):

```elm
type Input a
    = Mouse MouseEvent
    | Hover (Maybe a)

automaton : Maybe a -> Automaton (Input a) (Maybe ( a, Action ))
```

This can be used in specific situations where the `track`-functions
are not applicable. See
[Example4.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example4.elm)
([demo](https://jvoigtlaender.github.io/elm-drag/Example4.html)),
where the automaton is used to realize accurate dragging of
non-rectangular shapes.

(Said automaton is also used internally in the `track`-functions.)
