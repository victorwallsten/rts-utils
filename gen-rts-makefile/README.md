# gen-rts-makefile
A makefile generator for the laboratory project in Real-time Systems (LET627,
EDA223, DIT162) at Chalmers University of Technology.

## Dependencies
* [Stack](https://docs.haskellstack.org) - I recommend installing via
  [GHCup](https://haskell.org/ghcup)

## Install
Run `stack install` - the binary `gen-rts-makefile` will be copied to a location
based on your non-project-specific stack configuration (by default
`$HOME/.local/bin`).

## Usage
Run `gen-rts-makefile` from the root folder of the project (where
`application.c` and `TinyTimber.c` are located). Your own `.c` files should also
be located there. The generated makefile will be placed in the same folder.

It is assumed that the `arm-none-eabi-*` compiler directory is on the `PATH`
environment variable; if not, supply an argument to the program specifying the
full path to the directory along with the partial `arm-none-eabi` executable
name (e.g. run `gen-rts-makefile
$HOME/Downloads/gcc-arm-none-eabi-VERSION_NAME/arm-none-eabi`).

## Limitations
The parser is very primitive - essentially, it just looks for the word
`#include` followed by a word wrapped in quotation marks. It does not check
whether the statement is commented out, nor does it check whether the included
file actually exists.
