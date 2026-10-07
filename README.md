# A supplementary repository for metacomputations course

- Tool installation instructions:

  - Prerequisites: build tools (make, gcc, etc., consult your OS/environment
    docs)
  - Install opam [https://opam.ocaml.org/]
  - Install ostap: `opam install ostap`
  - Install GT: `opam install GT`

- Build instructions:

  - from within `src` subdirectory do `make`; this would build the `Imp` executable
  - do `make clean` to clean the directory up

- Run instructions:

  - `./Imp [options] file`
  - options are:
    - `--run` - run a program
    - `--spec` - specialize a program (not yet implemented)
    - `--input <input spec>` - specify initial state; input spec is a comma-separated
      list of variable names and their integer values
  - Example: `./Imp --run --input 'n=3, k=5' exp.imp` runs a problem from class.
    