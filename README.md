# fp-truncate

Tools and programs for truncating the number of bits in the significand of
floating-point numbers. The numbers can be represented in decimal, binary or
hexadecimal.

## Command-line programs

The command-line programs take the same set of arguments, one program is
provided for each input representation (e.g., 32-bit binary, 64-bit hexadecimal).
The first argument is the floating-point value to be truncated, expressed in the
appropriate representation. The programs are:

* `tr32b` - accepts 32-bit binary string inputs
* `tr64b` - accepts 64-bit binary string inputs
* `tr32h` - accepts 32-bit decimal string inputs
* `tr64h` - accepts 64-bit decimal inputs
* `tr32d` - accepts 32-bit hexadecimal inputs
* `tr64d` - accepts 64-bit hexadecimal inputs

The number of bits in the significand of the output is indicated with either 1
or 2 arguments. If one argument is provided it is interpreted as the number of
significand bits in the output, and the output will be a single line:

    tr32d 3.14159 5
    3.125

If two arguments are provided they are interpreted as a range of significand
sizes, resulting in one line of output for each significand size in the range:

    tr32d 3.14159 5 7
    3.125
    3.15625
    3.140625

or in reverse order:

    tr32d 3.14159 7 5
    3.140625
    3.15625
    3.125

A single option `-b` or `--base=` may be given, which specifies the base of the
program output:

    tr32d 3.14159 5 -b 2
    01000000010010000000000000000000

Valid bases are 2 (binary), 10 (decimal) and 16 (hexadecimal).

## The `fp-truncate` library

The backend used by the command-line programs can be used as a library to write
your own programs. The `fp-truncate` library exports the following modules:

* `Truncate` - The core truncation library
* `Truncate.Main` - A main program to implement the command-line programs
* `Truncate.Main.Readers` - Readers for processing command-line arguments
