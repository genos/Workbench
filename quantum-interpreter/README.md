# quantum-interpreter

A Rust implementation of `stylewarning's` _wonderful_ [tutorial quantum
interpreter](https://www.stylewarning.com/posts/quantum-interpreter/), with
some inspiration taken from [this OCaml
version](https://github.com/sheganinans/QVM-ocaml-mini).


```shell
$ cargo run --release --example bell
    <SNIP>
[526, 0, 0, 474]
$ cargo run --release --example ghz
    <SNIP>
[492, 0, 0, 0, 0, 0, 0, 508]
$ cargo run --release --example qft
    <SNIP>
[59, 67, 77, 50, 48, 74, 69, 57, 69, 73, 71, 54, 47, 63, 63, 59]
```
