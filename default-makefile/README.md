# default-makefile
A makefile for the default project.

## Dependencies
* `make`
* gcc-arm-none-eabi:
    * `arm-none-eabi-gcc`
    * `arm-none-eabi-as`
    * `arm-none-eabi-g++`
    * `arm-none-eabi-objcopy`
* GNU coreutils:
    * `test`
    * `mkdir`
    * `rm`

## Usage
To build the project, put the makefile in the root folder of the project (where
`application.c` and `TinyTimber.c` are located) and run `make` from there. It is
assumed that the compiler directory is on the `PATH` environment variable; if
not, edit the definition at [line 6](Makefile#L6), specifying the full path to
the directory along with the partial `arm-none-eabi` executable name (e.g.
`$HOME/Downloads/gcc-arm-none-eabi-VERSION_NAME/arm-none-eabi`).

When adding source files to the project, object names and corresponding rules
need to be added to the makefile. For example, if `foo.c` which depends on
`foo.h` is to be added, add `$(DEBUGDIR)foo.o` to the `OBJECTS` list starting at
[line 44](Makefile#L44) and the following rule to the `User-defined targets`
list starting at [line 106](Makefile#L106):
```
$(DEBUGDIR)foo.o: foo.c foo.h
	$(CC) -c $< -o $@ $(CCFLAGS)
```
Do note that `make` requires the recipe (`$(CC) -c $< ...`) to start with an
indentation made by a literal tab character, not spaces.
