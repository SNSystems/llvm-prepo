#*                        *
#*  _ __ ___ _ __   ___   *
#* | '__/ _ \ '_ \ / _ \  *
#* | | |  __/ |_) | (_) | *
#* |_|  \___| .__/ \___/  *
#*          |_|           *
#===- repo.cmake ----------------------------------------------------------===//
# Copyright (c) 2017-2018 by Sony Interactive Entertainment, Inc.
# All rights reserved.
#
# Developed by:
#   Toolchain Team
#   SN Systems, Ltd.
#   www.snsystems.com
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the
# "Software"), to deal with the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# - Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimers.
#
# - Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimers in the
#   documentation and/or other materials provided with the distribution.
#
# - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
#   Inc. nor the names of its contributors may be used to endorse or
#   promote products derived from this Software without specific prior
#   written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
# ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
#===----------------------------------------------------------------------===//

#this one not so much
SET (CMAKE_SYSTEM_VERSION 1)
SET (CMAKE_SYSTEM_PROCESSOR "x86_64")

set (triple "x86_64-pc-linux-gnu-repo")

# The user must specify CMAKE_C_COMPILER by giving '-DCMAKE_C_COMPILER:FILEPATH=/path/to/c/compiler' on the command line
SET (CMAKE_C_COMPILER   "clang"  CACHE FILEPATH "Compiler")
# The user must specify CMAKE_CXX_COMPILER by using '-DCMAKE_CXX_COMPILER:FILEPATH=/path/to/cxx/compiler' on the command line
SET (CMAKE_CXX_COMPILER "clang++"  CACHE FILEPATH "Compiler")

# The user must specify the utils_dir by giving '-Dutils_dir:STRING=/path/to/llvm/utils/repo' on the command line
if (utils_dir)
    # Environment variables are always preserved.
    set(ENV{_utils_dir} "${utils_dir}")
else ()
    set(utils_dir "$ENV{_utils_dir}")
endif ()

SET (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -fno-exceptions -fno-rtti" CACHE STRING "Default C Flags Debug")
SET (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -fno-exceptions -fno-rtti" CACHE STRING "Default CXX Flags Debug")
SET (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -fno-exceptions -fno-rtti" CACHE STRING "Default C Flags Release")
SET (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -fno-exceptions -fno-rtti" CACHE STRING "Default CXX Flags Release")
set (CMAKE_C_COMPILER_TARGET ${triple})
set (CMAKE_CXX_COMPILER_TARGET ${triple})

set (CMAKE_LINKER ${utils_dir}/link.py)
set (CMAKE_C_LINKER ${CMAKE_LINKER})
set (CMAKE_CXX_LINKER ${CMAKE_LINKER})

set (CMAKE_C_LINK_EXECUTABLE "${utils_dir}/link.py <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
set (CMAKE_CXX_LINK_EXECUTABLE "${utils_dir}/link.py <FLAGS> <CMAKE_CXX_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")

set (CMAKE_C_CREATE_SHARED_LIBRARY   "${utils_dir}/link.py <FLAGS> <CMAKE_SHARED_LIBRARY_C_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS> <SONAME_FLAG><TARGET_SONAME> -o <TARGET> <OBJECTS> <LINK_LIBRARIES>")
set (CMAKE_CXX_CREATE_SHARED_LIBRARY "${utils_dir}/link.py <FLAGS> <CMAKE_SHARED_LIBRARY_CXX_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS> <SONAME_FLAG><TARGET_SONAME> -o <TARGET> <OBJECTS> <LINK_LIBRARIES>")

set (CMAKE_AR "${utils_dir}/archive.py" CACHE FILEPATH "Archiver")

set (CMAKE_FIND_LIBRARY_PREFIXES "")
set (CMAKE_FIND_LIBRARY_SUFFIXES ".a")

# eof: repo.cmake
