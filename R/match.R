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



#' Match patterns in a string.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param string a character vector
#' @param value return value instead of bool result
#' @param anchor "start": anchor match at the beginning of the string, "both": anchor match at the beginning and the end of the string, "none": no anchor.
#' @param all find all matches instead of the first match. When result = "value", a matched character matrix will be returned.
#' @param ... further arguments passed to or from other methods.
#' @export
re2_match = function(pattern, string, value = FALSE, anchor = "none", all = FALSE, ...) UseMethod("re2_match")

#' @rdname re2_match
#' @export
re2_match.re2exp = function(pattern, string, value = FALSE, anchor = "none", all = FALSE, ...){
    cpp_match(pattern, string, value, anchor, all)
}

#' @rdname re2_match
#' @export
re2_match.character = function(pattern, string, value = FALSE, anchor = "none", all = FALSE, ...){
    pattern = re2(pattern, ...)
    re2_match.re2exp(pattern, string, value, anchor, all)
}
