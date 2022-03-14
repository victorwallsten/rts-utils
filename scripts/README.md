# scripts
Scripts for communicating with the MD407 via the terminal.

## Dependencies
* A terminal emulator.
* GNU coreutils:
    * `stty`
    * `cat`
    * `echo`
    * `sleep`

## Usage
To see the output from and transmit text to the MD407 (by means of mashing keys
and pressing enter), run the `rtshow` script from anywhere. Typically, the name
of the file denoting the connection is `/dev/ttyUSB0`. Hence, that is the
default file name concatenated by the script. If the numbering is different, a
number can be supplied as an argument to the script. For example, if the name of
the file is `/dev/ttyUSB1`, run `rtshow 1`. If the naming convention is entirely
different, edit the script accordingly. If the connection is refused due to file
permissions, either add the current user to the `dialout` group via `adduser
USERNAME dialout` or run the script with superuser privileges.

To send the `Debug/RTS-Lab.s19` binary to the MD407, run the `rtsend` script
from the root folder of the project (where `application.c` and `TinyTimber.c`
are located). The same file name assumption as previously mentioned is made in
this script, with the same default behaviour if no argument is supplied to the
script. To send the binary to one or more files with specific numbers, those
numbers can be supplied as arguments to the script. For example, to send the
binary to `/dev/ttyUSB0` and `/dev/ttyUSB1`, run `rtsend 0 1`.
