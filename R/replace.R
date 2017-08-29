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


#' Replace matched patterns in a string.
#'
#' Replace the the first match or all matches of \code{pattern} in \code{string} with \code{replacement}.
#'
#' Within \code{replacement}, backslash-escaped digits (\\1 to \\9) can be
#' used to insert text matching corresponding parenthesized group
#' from the pattern.  \\0 in \code{replacement} refers to the entire matching
#' text.
#'
#' Vectorised over strings, patterns and replacements.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param replacement replace the first match or all of the match of \code{pattern} in \code{string} with "rewrite"
#' @inheritParams re2_locate
#' @examples
#' # replace one or more b, prefer more
#' regexp = re2("b+")
#' re2_replace_all("yabba dabba doo", regexp,"d")
#' re2_replace("yabba dabba doo", "b+","d")
#'
#' # trim string
#' pattern = "^\\s+|\\s+$"
#' re2_replace_all(c("  abc  "," this is "), pattern, "")
#'
#' # mask the middle three digits of a US phone number
#' texts = c("415-555-1234",
#'           "650-555-2345",
#'           "(416)555-3456",
#'           "202 555 4567",
#'           "4035555678",
#'           "1 416 555 9292")
#'
#' us_phone_pattern = re2("(1?[\\s-]?\\(?\\d{3}\\)?[\\s-]?)(\\d{3})([\\s-]?\\d{4})")
#'
#' re2_replace(texts, us_phone_pattern, "\\1***\\3")
#'
#' # show_regex(us_phone_pattern)
#'
#' @return For \code{\link{re2_replace}}, a character vector. For \code{\link{re2_replace_all}}, a character vector with the number of replacements.
#' @export
re2_replace = function(string,
                       pattern,
                       replacement,
                       parallel = FALSE,
                       grain_size = 100000,
                       ...) {
    if (is.character(pattern) || mode(pattern) == "logical") {
        pattern = re2(pattern, ...)
    }
    cpp_replace(
        stri_enc_toutf8(string),
        pattern,
        stri_enc_toutf8(replacement),
        FALSE,
        parallel ,
        grain_size
    )
}

#' @rdname re2_replace
#' @export
re2_replace_all = function(string,
                           pattern,
                           replacement,
                           parallel = FALSE,
                           grain_size = 100000,
                           ...) {
    if (is.character(pattern) || mode(pattern) == "logical") {
        pattern = re2(pattern, ...)
    }
    cpp_replace(
        stri_enc_toutf8(string),
        pattern,
        stri_enc_toutf8(replacement),
        TRUE,
        parallel ,
        grain_size
    )
}
