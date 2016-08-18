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

#' Find matched groups from strings.
#'
#' @inheritParams re2_count
#' @examples
#'
#' strings <- c("Gym: 627-112-1433", "Apple x2",
#'              "888 888 8888", "This is a test.",
#'              "627-112-1433 223-343-2232")
#' phone <- "([2-9][0-9]{2})[- .](?P<second>[0-9]{3})[- .]([0-9]{4})"
#' re2_extract(strings, phone)
#' re2_match(strings, phone)
#'
#' re2_extract_all(strings, phone)
#' re2_match_all(strings, phone)
#'
#' regexp = re2("test",case_sensitive = FALSE)
#' re2_match("TEST", regexp)
#'
#' # differences from stringi
#'
#' # This kind of repeating capturing group works differently.
#' re2_match("aasd", "(a*)+")
#' stringi::stri_match("aasd", regex = "(a*)+")
#'
#' # In stringi, "" empty search patterns return NA.
#' # In re2r, empty search patterns will match
#' # empty string.
#'
#' re2_match("abc", "")
#' stringi::stri_match("abc", regex = "")
#'
#' @return For \code{\link{re2_match}}, a character matrix. First column
#'  is the complete match, followed by one column
#' for each capture group with names.
#'
#' For \code{\link{re2_match_all}}, a list of
#' character matrices.
#' @export
re2_match = function(string,
                     pattern,
                     anchor = UNANCHORED,
                     parallel = FALSE,
                     grain_size = 100000,
                     ...) {
    if (is.character(pattern) || mode(pattern) == "logical") {
        pattern = re2(pattern, ...)
    }
    cpp_match(stri_enc_toutf8(string),
              regexp = pattern,
              value = TRUE,
              anchor = anchor,
              all = FALSE,
              parallel = parallel,
              grain_size = grain_size)
}

#' @export
#' @rdname re2_match
re2_match_all = function(string,
                         pattern,
                         anchor = UNANCHORED,
                         parallel = FALSE,
                         grain_size = 100000,
                         ...) {
    if (is.character(pattern) || mode(pattern) == "logical") {
        pattern = re2(pattern, ...)
    }
    cpp_match(stri_enc_toutf8(string),
              regexp = pattern,
              value = TRUE,
              anchor = anchor,
              all = TRUE,
              parallel = parallel,
              grain_size = grain_size)
}
