# LLVM with Program Repository Support

This git repository contains a copy of LLVM (forked from commit 57bb7d2d7058512fff60f58c343aad15eae64afb) with work-in-progress modifications to output to a Program Repository.

The changes are to add support for the program repository that was first shown at the [2016 US LLVM Developers’ meeting](https://llvm.org/devmtg/2016-11/) in the talk catchily titled “Demo of a repository for statically compiled programs”. You can relive the highs and lows by [watching it on YouTube](https://youtu.be/-pL94rqyQ6c). The early implementation demonstrated there has its [own Github repository](https://github.com/SNSystems/Toy-tools): in essence, this work re-implements the same thing in LLVM to give you the anticipated build-time improvements in a C++ compiler targetting Linux.

## Building the Compiler

The process to follow is similar to that for a conventional build of Clang+LLVM, but with with the Clang and LLVM sources coming from different repositories and an extra step to get the [pstore](https://github.com/SNSystems/pstore) back-end.

1. Clone llvm-prepo (this repository):

        $ git clone https://github.com/SNSystems/llvm-prepo.git

1. Clone [clang-prepo](https://github.com/SNSystems/clang-prepo) (forked from commit d6aa2b1c1a49068f3c8d5405083fde4118d75e4f):

        $ cd llvm/tools
        $ git clone https://github.com/SNSystems/clang-prepo.git
        $ cd -

1. Clone [pstore](https://github.com/SNSystems/pstore):

        $ cd llvm/tools
        $ git clone https://github.com/SNSystems/pstore.git
        $ cd -
        
   Ultimately, we envisage supporting multiple database back-ends to fit different needs, but there’s currently a hard dependency on the pstore (“Program Store”) key/value store as a back-end.


1. Build LLVM as [normal](https://llvm.org/docs/CMake.html).

## Using the Program Repository

### Compiling
The program-repository is implemented as a new object-file format (“repo”) in LLVM. To use it, you need to request it explicitly in the target triple:

    $ clang -target x86_64-pc-linux-gnu-repo -o test.o test.c

Note that this is the only triple that we’re currently supporting (i.e. targeting X86-64 Linux).

Furthermore, the path to the program-repository database itself is set using an environment variable `REPOFILE`; it that variable is not set, it defaults to `./clang.db` (eventually, you’d expect to be able to specify this path with a command-line switch).

The command-line above will write the object code for `test.c` to the program-repository and emit a “ticket file” `test.o`. This tiny file contains a key to the real data in the database.

### Linking
A program-repository aware linker is very much on the project’s [“TODO” list](wiki/Limitations#missing-features). Until that happens, there's a `repo2obj` tool in the project tree. This generates a traditional ELF file from a repository ticket file. Using it is simple:

    $ clang -target x86_64-pc-linux-gnu-repo -c -o test.o test.c
    $ repo2obj test.o -o test.o.elf
    
The first step compiles the source file `test.c` to the repository, the second will produce a file `test.o.elf` which can be fed to a traditional ELF linker.

    $ clang -o test test.o.elf
    $ ./test

Low Level Virtual Machine (LLVM)
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


