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


#' Extract matching patterns from a string.
#'
#' Extract matching patterns from a string. Vectorised over string and pattern.
#'
#' @inheritParams re2_count
#' @examples
#' re2_extract("yabba dabba doo", "(.)")
#' re2_extract_all("yabba dabba doo", "(.)")
#'
#' str <- c("Aster", "Azalea x2", "Baby's Breath", "Bellflower")
#' re2_extract(str, "\\d")
#' re2_extract(str, "[a-z]+")
#' re2_extract(str, "\\b\\w{1,3}\\b")
#'
#' # Extract all matches
#' re2_extract_all(str, "[A-Za-z]+")
#' re2_extract_all(str, "\\b\\w{1,3}\\b")
#' re2_extract_all(str, "\\d")
#'
#' @return A character vector for \code{\link{re2_extract}}, and a list for \code{\link{re2_extract_all}}.
#' @seealso \code{\link{re2_match}} to extract matched groups.
#' @export
re2_extract = function(string,
                       pattern,
                       anchor = UNANCHORED,
                       parallel = FALSE,
                       grain_size = 100000,
                       ...) {
    if (is.character(pattern) || mode(pattern) == "logical") {
        pattern = re2(pattern, ...)
    }
    cpp_extract(stri_enc_toutf8(string), pattern, all = FALSE, anchor = anchor,  FALSE, 1)
}

#' @rdname re2_extract
#' @export
re2_extract_all = function(string,
                           pattern,
                           anchor = UNANCHORED,
                           parallel = FALSE,
                           grain_size = 100000,
                           ...) {
    if (is.character(pattern) || mode(pattern) == "logical") {
        pattern = re2(pattern, ...)
    }
    cpp_extract(stri_enc_toutf8(string), pattern, all= TRUE, anchor = anchor, FALSE, 1)
}
