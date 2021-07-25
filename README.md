# Achtung, die Kurv clone written in Clojure

A hobby project, implementing a [clone of the original game](https://achtungdiekurve.net/) in Clojure.

Has a ClojureScript renderer, so to make it playable in the browser. The game engine, state and logic are in a .cljc namespace,
so it's possible to add a JavaFX renderer for instance.


## Architecture

### Game engine / logic

The game engine can be started from the REPL. It returns a map with 3 core async channels.
1) `render-chan` A read-only channel that should be continuously read from by the caller, reflecting the latest game state. The caller can then render this as it sees fit.
2) `kill-chan` a channel where, if sent a truthy value, will restart the game state.
3) `key-chan` a write-only channel where the caller should write keyboard events to. A keyboard event should be a keyword of the letter pressed, ie. :x if the x key was pressed.

Internally, it uses a timeout channel for keeping a fixed 60fps clock tick going. If there's multiple inputs between ticks, it buffers those, and then when the next tick arrives, moves the game state forwards based on those inputs

The gamestate is just a normal reduction (ie. is `reduce`d over, using the keyboard/time events as input). When a clock tick has been done, it sends the game state to the render channel.

NB: Yes, it kind of assumes the rendering function is done in less than 1/60th of a second. Maybe this should be improved.
