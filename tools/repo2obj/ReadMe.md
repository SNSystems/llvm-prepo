# repo2obj

repo2obj is a utility which extracts an ELF object file from a Program Repository. An object file produced by this tool should be functionally identical to an object file emitted directly by the compiler.

Its command-line interface is very simple:

    repo2obj [options] <ticket file path>

As with the compiler, by default `repo2obj` looks for the database itself at the location given by the `REPOFILE` environment variable (defaulting to `./clang.db` if the variable is not set). This may be overriden using the `--repo` switch.
