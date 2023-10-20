# Changelog for `thetundra`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).


## Unreleased

### Features

- added rooms
- added up and down as directions
- added enter and exit aliases
- added push cmd
- configured Object to allow specification of action to do for an object command, rather than just a string to return

### Fixes

## 0.1.1 - 2023-XX-XX

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
