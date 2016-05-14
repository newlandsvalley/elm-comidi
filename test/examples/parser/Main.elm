import Parser exposing (init, update, view)
import Html.App as Html

main = 
  Html.program
    { init = init "midi file parser", update = update, view = view, subscriptions = \_ -> Sub.none }


