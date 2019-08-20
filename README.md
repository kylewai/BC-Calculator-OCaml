# BC-Calculator-OCaml
An OCaml interpreter for major features of the bc language. <br><br>

This project does not include a parser or a lexer. As such, the components of the "bc language" such as variables, for loops, and functions are represented by user-defined types. <br><br>

With ocaml installed, the code can be run with:
* ocamlfind ocamlc -package core -linkpkg -thread -o bc bc.ml && ./bc
