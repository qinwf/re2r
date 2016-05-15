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


#' Replace a pattern in strings.
#'
#' Replace the first match of "pattern" in "str" with "rewrite".
#' Within "rewrite", backslash-escaped digits (\\1 to \\9) can be
#' used to insert text matching corresponding parenthesized group
#' from the pattern.  \\0 in "rewrite" refers to the entire matching
#' text.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param rewrite replace the first match or all of the match of "pattern" in "input" with "rewrite"
#' @param input a character vector
#' @param ... further arguments passed to or from other methods.
#' @return a character vector
#' @examples
#' regexp = re2("b+")
#' re2_replace_all("yabba dabba doo", regexp,"d") == "yada dada doo"
#' re2_replace("yabba dabba doo", "b+","d") == "yada dabba doo"
#' @export
re2_replace = function(input, pattern, rewrite, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_replace(stri_enc_toutf8(input), pattern, stri_enc_toutf8(rewrite),  FALSE, FALSE,1)
}

#' @rdname re2_replace
#' @export
re2_replace_all = function(input, pattern, rewrite, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_replace(stri_enc_toutf8(input), pattern, stri_enc_toutf8(rewrite), TRUE, FALSE,1)
}

#' Replace a pattern in strings with multithread.
#'
#' Replace the first match of "pattern" in "str" with "rewrite".
#' Within "rewrite", backslash-escaped digits (\\1 to \\9) can be
#' used to insert text matching corresponding parenthesized group
#' from the pattern.  \\0 in "rewrite" refers to the entire matching
#' text.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param rewrite replace the first match or all of the match of "pattern" in "input" with "rewrite"
#' @param input a character vector
#' @param grain_size a minimum chunk size for tuning the behavior of parallel algorithms.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a character vector
#' @examples
#' regexp = re2("b+")
#' re2_preplace_all("yabba dabba doo", regexp,"d") == "yada dada doo"
#' re2_preplace("yabba dabba doo", "b+","d") == "yada dabba doo"
#' @export
re2_preplace = function(input, pattern, rewrite , grain_size = 100000, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_replace(stri_enc_toutf8(input), pattern, stri_enc_toutf8(rewrite),  FALSE, TRUE, grain_size)
}

#' @rdname re2_preplace
#' @export
re2_preplace_all = function(input, pattern, rewrite, grain_size = 100000, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_replace(stri_enc_toutf8(input), pattern, stri_enc_toutf8(rewrite),  TRUE, TRUE, grain_size)
}
