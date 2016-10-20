# ROOPLC
ROOPL to PISA compiler

Use the Pendulum VM to execute the PAL files

### Building

Simply invoke GHC (7.10.3+) on /src/ROOPLC.hs

No Cabal files, sorry!

### Running

The compiler reads input from stdin until it reaches EOF, it then compiles the code and prints the output to stdout, errors are printed to stderr.

On UNIX/Linux:
    
    cat ./test/fibonacci.rpl | ./src/ROOPLC > ./test/fibonacci.pal

On Windows:

    type .\test\fibonacci.rpl | .\src\ROOPLC.exe > .\test\fibonacci.pal

__NOTE__: Both PowerShell and cmd.exe will output UTF-16 LE w/ BOM by default, which is incompatible with PendVM.

__NOTE__: PendVM only allocates 16 bytes for each instruction label, but ROOPLC does not limit itself to 16 characters per label.
