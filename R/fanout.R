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


#' Get program fanout
#'
#' Return the program fanout as a histogram bucketed by powers of 2.
#' @param regexp a pre-compiled regular expression
#' @examples
#' re1 = re2("(?:(?:(?:(?:(?:.)?){1})*)+)")
#' re10 = re2("(?:(?:(?:(?:(?:.)?){10})*)+)")
#' re100 = re2("(?:(?:(?:(?:(?:.)?){100})*)+)")
#' re1000 = re2("(?:(?:(?:(?:(?:.)?){1000})*)+)")
#'
#' get_program_fanout(re1)
#' get_program_fanout(re10)
#' get_program_fanout(re100)
#' get_program_fanout(re1000)
#' @export
get_program_fanout = function(regexp) {
    res = cpp_get_program_fanout(regexp)
    if (is.null(res)) return (data.frame(index = NA, value= NA))
    return(data.frame(index = as.numeric(names(res)),
                      value = as.numeric(res)))
}
