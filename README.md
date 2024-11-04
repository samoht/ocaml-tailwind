# ocaml-tailwind

The long-term goal of ocaml-tailwind is to enable the generation of
Tailwind-compatible CSS code directly within OCaml. This would allow
developers to use Tailwind in pure OCaml environments without needing
the Tailwind CLI, making it especially useful on systems where the CLI
may not run smoothly, such as FreeBSD.

## Current Status

`dune exec -- src/tailwind.exe <file>` currently parses a CSS file
produced by Tailwind CSS. This can be used to generate code.
