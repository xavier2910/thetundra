# thetundra

A silly text adventure game in the series of [The Cave and The Forest](https://github.com/xavier2910/thecaveANDtheforest). However,
this one has a command processor instead of a list of options. Which is a trade off, and ratehr more difficult than first expected. Also,
I don't like how I made the world an infinitely deep data structure: once you can change the world (think `take` or `put` commands), it'll
get rather inefficient. It's actually so bad I've decided to cease work on it entirely and [start from scratch in another language](github.com/xaiver2910/tundra).

## Get Started

Clone/fork/download the code
You should be able to build using stack in the repo directory (`stack build`)
The executable is called just `thetundra` (*not* `thetundra-exe`), it can be
run from stack as per usual (`stack exec thetundra`)

If you don't want to get your hands dirty with the code, check out the [releases page](https://github.com/xavier2910/thetundra/releases).
