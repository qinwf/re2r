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

#' Extract and replace substrings from a character vector.
#'
#' \code{sub_string} extracts substrings under code point-based index ranges provided. \code{sub_string<-} allows to substitute parts of a string with given strings.
#'
#' @param string input character vector.
#' @param from an integer vector or a two-column matrix.  \code{from} gives the position of the first character (defaults to first). Negative values count backwards from the last character.
#' @param to an integer vector. \code{to} gives the position of the last (defaults to last character).
#' @param value replacement string
#' @return A character vector of substring from \code{start} to \code{end}
#'   (inclusive). Will be length of longest input argument.
#' @seealso The underlying implementation in \code{\link[stringi]{stri_sub}}
#' @examples
#' sub_string("test", 1, 2)
#'
#' x <- "ABC"
#'
#' (sub_string(x, 1, 1) <- "A")
#' x
#' (sub_string(x, -2, -2) <- "GHIJ")
#' x
#' (sub_string(x, 2, -2) <- "")
#' x
#' @export
sub_string <- function(string, from = 1L, to = -1L) {
    if (is.matrix(from)) {
        stringi::stri_sub(string, from = from)
    } else {
        stringi::stri_sub(string, from = from, to = to)
    }
}


#' @export
#' @rdname sub_string
"sub_string<-" <- function(string, from = 1L, to = -1L, value) {
    if (is.matrix(from)) {
        stringi::stri_sub(string, from = from) <- value
    } else {
        stringi::stri_sub(string, from = from, to = to) <- value
    }
    string
}
