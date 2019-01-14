# potential-doodle

Potential-doodle is intended to be a program into which one can fairly easily input a description of a language together with a bilingual dictionary which can then be used to generate translations of various annotated texts or to generate random sentences and their translations for various purposes, particularly amusement and attempting to become fluent.

At present the system appears to have a relatively complete backend and command-line interface in [doodle.py](doodle.py), but the documentation has barely been started and the various attempts to make interfaces are fragmentary and severly outdated.

An outline of the repository's structure can be found at [Map.md](Map.md) and the current todo list at [TODO.md](TODO.md).

## Example Usage

- Ensure that lttoolbox and hfst are installed
- Download https://github.com/apertium/apertium-spa/blob/master/apertium-spa.spa.dix as langs/7/lang.dix
- run ```./maketransducer.sh 1``` and ```./maketransducer-lt.sh 7```
- run ```./doodle.py -d texts/2john.pdtxt 2john_eng.txt -t texts/2john.pdtxt 7 2john_esp.pdtxt -d 2john_esp.pdtxt 2john_esp.txt```

## Dependencies

- [HFST](http://hfst.github.io)
  - The Python version also interfaces with Lttoolbox, which is part of [Apertium](http://www.apertium.org).
- Lisp Libraries
  - [Quicklisp](https://www.quicklisp.org/beta/)
  - [CL-PPCRE](https://edicl.github.io/cl-ppcre/)
  - [UIOP](https://gitlab.common-lisp.net/asdf/asdf/blob/master/uiop/README.md)
