## This file is part of the 're2r' package for R.
## Copyright (C) 2016, Qin Wenfeng
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## 1. Redistributions of source code must retain the above copyright notice,
## this list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
##
## 3. Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
## BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
## FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
## OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
## EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#' Extract one matched patterns in a string.
#'
#' Like Replace, except that if the pattern matches, "rewrite"
#' is copied into "out" with substitutions.  The non-matching
#' portions of "text" are ignored.
#'
#' Returns true iff a match occurred and the extraction happened
#' @param pattern a pre-compiled regular expression or a string
#' @param rewrite replace the first match of "pattern" in "input" with "rewrite"
#' @param input a character vector
#' @param parallel multithreading support
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_extract("yabba dabba doo", "(.)")
#' re2_extract("test@me.com", "(.*)@([^.]*)", "\\2!\\1")
#' @export
re2_extract = function(input, pattern, rewrite = "\\1", parallel = FALSE, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_extract(stri_enc_toutf8(input), pattern, stri_enc_toutf8(rewrite), parallel)
}
