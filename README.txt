LLVM with Program Repository Support
====

This repository contains a copy of LLVM (forked from commit: 81b03a38899a9e94b2a8cbf07dac58473b97cedc) with work-in-progress modifications to output to a Program Repository. 

The changes are to add support for the Program Repository that was first shown at the 2016 US LLVM Developers’ meeting in the catchily titled “Demo of a repository for statically compiled programs”. You can relive the highs and lows of that talk by [watching it on YouTube](https://youtu.be/-pL94rqyQ6c). The implementation demonstrated in that talk has its own Github repository at <https://github.com/SNSystems/Toy-tools>: in essence, this work re-implements the same thing in LLVM to give you the build-time improvements in a C++ compiler.

Building the Compiler
---
The process to follow is the same as that for a conventional build of Clang+LLVM, but with a couple of extra step to get both the matching clang revision and the [pstore](https://github.com/SNSystems/pstore) back-end.

1. Clone llvm-prepo and clang:

        $ git clone https://github.com/SNSystems/llvm-prepo.git

1. Check out clang

        $ cd llvm/tools
        $ git clone http://llvm.org/git/clang.git
        $ cd clang
        $ git reset —hard 2276925224353681293a6a8ff6758a984d9c2e1c

1. Clone pstore:

        $ cd llvm/tools
        $ git clone git@github.com:SNSystems/pstore.git

4. Build LLVM as normal

Note that you need a version of clang that was current when this repository was forked (hence the `git reset -hard`). Ultimately, I envisage supporting multiple database back-ends to fit different needs, but there's currently a hard dependency on the pstore (“Program Store”) key/value store as a back-end.

Using the Program Repository
---

The Program Repository is implements as a new object-file format (“repo”) in LLVM. To use it, it's necessary to state that explicitly in the target triple:

    $ clang ‑target x86_64‑pc‑linux‑gnu‑repo -o test.o test.c

Furthermore, the repository path is currently hard-wired as `./clang.db` (eventually, you'd expect to be able to specify it with a command-line switch).


The LLVM Compiler Infrastructure
================================

This directory and its subdirectories contain source code for LLVM,
a toolkit for the construction of highly optimized compilers,
optimizers, and runtime environments.

LLVM is open source software. You may freely distribute it under the terms of
the license agreement found in LICENSE.txt.

Please see the documentation provided in docs/ for further
assistance with LLVM, and in particular docs/GettingStarted.rst for getting
started with LLVM and docs/README.txt for an overview of LLVM's
documentation setup.

If you are writing a package for LLVM, see docs/Packaging.rst for our
suggestions.

