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


#' Locate a pattern in strings.
#'
#' If the match is of length 0, (e.g. from a special match like $) end will be one character less than start.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_locate("yabba dabba doo", "d")
#' @export
re2_locate = function(input, pattern, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_locate(stri_enc_toutf8(input), pattern, FALSE, FALSE)
}

#' @rdname re2_locate
#' @export
re2_locate_all = function(input, pattern, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_locate(stri_enc_toutf8(input), pattern, TRUE, FALSE)
}

#' Locate a pattern in strings with multithread.
#'
#' Like re2_preplace, except that if the pattern matches, substring is copied into the result. The non-matching
#' portions of "text" are ignored.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_plocate("yabba dabba doo", "(d)")
#' @export
re2_plocate = function(input, pattern, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_locate(stri_enc_toutf8(input), pattern, FALSE, TRUE)
}

#' @rdname re2_pextract
#' @export
re2_plocate_all = function(input, pattern, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_locate(stri_enc_toutf8(input), pattern, TRUE, TRUE)
}
