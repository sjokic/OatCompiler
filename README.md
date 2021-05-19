# Oat Compiler
Compiler for a restricted C-like language (Oat) written in OCaml for the Compiler Design course @ ETHZ.

The compiler consists of a frontend (Oat -> LLVMlite) and backend (LLVMlite -> X86lite). For the compilation of X86lite, Clang's backend is used.

For further details on the Oat language please see [here](https://www.cis.upenn.edu/~cis341/20sp/hw/hw04/oat.pdf).

In order to run the compiler, please see [here](https://github.com/sw9/oat-v1-compiler#use) and make sure the following are installed:
  - LLVM
  - GCC
  - Clang <= 3.6
  - OCaml >= 4.01.0
  - Menhir
