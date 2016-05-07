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

#' Split strings with a pattern.
#'
#' Split up a string into pieces.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @param part number of pieces to return. Default (Inf) uses all possible split positions.
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_split("yabba dabba doo", " ")
#' @export
re2_split = function(input, pattern, part = Inf, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_split(stri_enc_toutf8(input), pattern, part, FALSE,FALSE)
}

#' @rdname re2_split
#' @export
re2_split_fixed = function(input, pattern, part = Inf, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_split(stri_enc_toutf8(input), pattern, part, TRUE, FALSE)
}

#' Split strings with a pattern with multithread.
#'
#' Split up a string into pieces.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @param part number of pieces to return. Default (Inf) uses all possible split positions.
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_psplit("yabba dabba doo", " ")
#' @export
re2_psplit = function(input, pattern, part = Inf, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_split(stri_enc_toutf8(input), pattern, part, FALSE, TRUE)
}

#' @rdname re2_psplit
#' @export
re2_psplit_fixed = function(input, pattern, part = Inf, ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_split(stri_enc_toutf8(input), pattern, part, TRUE, TRUE)
}
