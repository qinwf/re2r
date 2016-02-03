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
#' Replace the first match of "pattern" in "str" with "rewrite".
#' Within "rewrite", backslash-escaped digits (\\1 to \\9) can be
#' used to insert text matching corresponding parenthesized group
#' from the pattern.  \\0 in "rewrite" refers to the entire matching
#' text.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param rewrite replace the first match or all of the match of "pattern" in "input" with "rewrite"
#' @param input a character vector
#' @param global if it is TRUE, it will replaces successive non-overlapping occurrences
#' @param ... further arguments passed to or from other methods.
#' @return a character vector
#' @examples
#' regexp = re2("b+")
#' re2_replace(regexp,"d", "yabba dabba doo") == "yada dada doo"
#' re2_replace("b+","d", "yabba dabba doo", global = FALSE) == "yada dabba doo"
#' @export
re2_replace = function(pattern, rewrite, input, global = FALSE, ...) UseMethod("re2_replace")

#' @rdname re2_replace
#' @export
re2_replace.re2exp = function(pattern, rewrite, input, global = FALSE, ...){
    # if (check_windows_strings(input)) input = enc2utf8(input)
    # if (check_windows_strings(rewrite))  rewrite = enc2utf8(rewrite)

    res = cpp_replace(pattern, rewrite, input, global)

    # if (update_windows_strings()) {
    #     Encoding(res) = "UTF-8"
    # }
    return(res)
}

#' @rdname re2_replace
#' @export
re2_replace.character = function(pattern, rewrite, input, global = FALSE, ...){
    pattern = re2(pattern, ...)
    re2_replace.re2exp(pattern, rewrite, input, global)
}
