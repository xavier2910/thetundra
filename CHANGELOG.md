# Changelog for `thetundra`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Internal:

- The location tree is no longer infinitely deep; there is now only one instance of every 
location, rather than one being generated inside further locations. This was done with
an eye to performance down the line: each change ever made to the gamestate \(other than 
location\) would have to be reapplied every time the player changed location. Whether this
is worth the complexity entailed is an open question. `non-infinite` is now a secondary
trunk with master list.


## 0.1.1

### Features:

- `go` command is now redundant, you can just type a direction.
- `examine` command functions.
- Much more \(examinable\) content added to existing locations.

### Fixes:

- Fixed `go` with no args crashing with `Prelude.head: empty list`.


## 0.1.0 - 2023-01-12

### Initial release:

- Supports commands look, go, & help.
- Contains five locations (pathetic, I know).
- Auto-wraps displayed messages.
- And not much else. But hey, it works!
