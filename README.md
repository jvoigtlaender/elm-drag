elm-drag-and-drop
=================

A low- and high-level interface to mouse drag and drop actions in [Elm](http://elm-lang.org/).
For documentation, see also http://package.elm-lang.org/packages/jvoigtlaender/elm-drag-and-drop/latest/DragAndDrop.

The low-level interface is:

```
type MouseEvent = StartAt (Int,Int) | MoveFromTo (Int,Int) (Int,Int) | EndAt (Int,Int)

mouseEvents : Signal MouseEvent
```

The recommended, high-level interface consists of the `track`-functions:

```
type Action = Lift | MoveBy (Int,Int) | Release

track : Bool -> Signal Bool -> Signal (Maybe Action)

trackMany : Maybe a -> Signal (Maybe a) -> Signal (Maybe (a, Action))
```

In those `track`-functions, the `Bool`/`Signal Bool` or `Maybe a`/`Signal (Maybe a)` arguments are the initial value and input signal which tell whether the mouse is (currently) hovering over something draggable. See `Example1.elm`, `Example2.elm`, and `Example3.elm`.

The library also exposes an `Automaton` (http://package.elm-lang.org/packages/evancz/automaton/latest):

```
type Input a = Mouse MouseEvent | Hover (Maybe a)

automaton : Maybe a -> Automaton (Input a) (Maybe (a, Action))
```

This can be used in specific situations where the `track`-functions are not applicable. See `Example4.elm`, where the automaton is used to realize accurate drag and drop of non-rectangular shapes.

(Said automaton is also used internally in the `track`-functions.)
