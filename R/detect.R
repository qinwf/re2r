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


#' Test a pattern in strings, and return boolean.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param string a character vector
#' @param anchor UNANCHORED: no anchor. ANCHOR_START: anchor match at the beginning of the string. ANCHOR_BOTH: anchor match at the beginning and the end of the string.
#' @param parallel use multithread
#' @param grain_size a minimum chunk size for tuning the behavior of parallel algorithms
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_detect("one", "(o.e)")
#' re2_detect("123-234-2222", "\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d")
#'
#' words = c("sunny","beach","happy","really")
#' re2_detect(words, "y")
#' re2_detect(words, "^b")
#' re2_detect(words, "[abc]")
#'
#' # vectorize
#' (res = re2_detect("This", letters))
#' letters[res]
#'
#' @export
re2_detect = function(string,
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
              value = FALSE,
              anchor = anchor,
              all = FALSE,
              parallel = parallel,
              grain_size = grain_size)
}
