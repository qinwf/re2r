cmake_minimum_required(VERSION 3.3)
project(re2r)

SET( CMAKE_VERBOSE_MAKEFILE on )

option(BUILD_SHARED_LIBS "build shared libraries" OFF)
option(USEPCRE "use PCRE in tests and benchmarks" OFF)

set(EXTRA_TARGET_LINK_LIBRARIES)

if(WIN32)
  add_definitions(-DUNICODE -D_UNICODE -DSTRICT -DNOMINMAX)
  set(THREADING threadwin)
else()
  add_definitions(-pthread)
  set(THREADING thread)
  list(APPEND EXTRA_TARGET_LINK_LIBRARIES -pthread)
endif()

set(RE2R_LIBRARY_SOURCES
	# re2/
    src/bitstate.cc
    src/compile.cc
    src/dfa.cc
    src/filtered_re2.cc
    src/mimics_pcre.cc
    src/nfa.cc
    src/onepass.cc
    src/parse.cc
    src/perl_groups.cc
    src/prefilter.cc
    src/prefilter_tree.cc
    src/prog.cc
    src/re2.cc
    src/regexp.cc
    src/set.cc
    src/simplify.cc
    src/stringpiece.cc
    src/tostring.cc
    src/unicode_casefold.cc
    src/unicode_groups.cc
	# util/
    src/hash.cc
    src/logging.cc
    src/rune.cc
    src/stringprintf.cc
    src/strutil.cc
    src/valgrind.cc
    # wrap
    src/re2r_wrap.cpp
    )

add_library(re2r SHARED ${RE2R_LIBRARY_SOURCES})

set(CMAKE_SHARED_LINKER_FLAGS "-s -static-libgcc -LC:/PROGRA~1/R/R-EXPE~1/bin/x64")

include_directories(C:/PROGRA~1/R/R-EXPE~1/include)
include_directories(./inst/include)
include_directories(C:/Users/outwen/Documents/R/win-library/3.3/Rcpp/include)

link_directories(C:/Rbuild/R-devel-win64/extsoft/lib)
link_directories(C:/PROGRA~1/R/R-EXPE~1/bin/x64)
link_directories(C:/Rbuild/R-devel-win64/extsoft/lib/x64)

target_link_libraries(re2r R)



