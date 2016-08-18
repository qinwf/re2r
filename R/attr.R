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

check_pattern = function(pattern, ...){
    stopifnot(length(pattern) == 1)
    if (is.character(pattern) || mode(pattern) == "logical") {
        pattern = re2(pattern, ...)
    }
    pattern
}

#' Simplify pattern strings.
#'
#' Simplify pattern strings.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param ... further arguments passed to \code{\link{re2}}
#' @examples
#' get_simplify("a{1}")
#' get_simplify("a{3}b+(:?abc(a))")
#' get_simplify("a{2,3}a{2}")
#' get_simplify(re2("1+2",literal = TRUE))
#' get_pattern(re2("1+2",literal = TRUE))
#' @export
get_simplify = function(pattern, ...) {
    cpp_regex_simplify(check_pattern(pattern))
}


#' Get program fanout
#'
#' Return the program fanout as a histogram bucketed by powers of 2.
#' @inheritParams get_simplify
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
get_program_fanout = function(pattern, ...) {
    res = cpp_get_program_fanout(check_pattern(pattern, ...))
    if (is.null(res)) return (data.frame(index = NA, value= NA))
    return(data.frame(index = as.numeric(names(res)),
                      value = as.numeric(res)))
}

#' Return the number of capturing subpatterns
#'
#' Return the number of capturing subpatterns, or -1 if the
#' regexp wasn't valid on construction.  The overall match ($0)
#' does not count: if the regexp is "(a)(b)", returns 2.
#'
#' @inheritParams get_simplify
#' @return a integer
#' @examples
#' regexp = re2("1")
#' get_number_of_groups(regexp)
#'
#' get_number_of_groups(re2("((?P<a>123)(12))"))
#'
#' # uncaptured groups
#' get_number_of_groups(re2("(?:(?:(?:(?:(?:.)?){100})*)+)"))
#' @export
get_number_of_groups = function(pattern, ...){
    cpp_get_groups(check_pattern(pattern, ...))
}


#' The string specification for this RE2.
#'
#' @inheritParams get_simplify
#' @examples
#' regexp = re2("1")
#' get_pattern(regexp)
#'
#' get_pattern(re2("^(?P<abc>abc)a"))
#' @return a string
#' @export
get_pattern = function(pattern, ...) {
    cpp_get_pattern(check_pattern(pattern, ...))
}

#' Return capturing names for a pre-compiled regular expression.
#'
#' Return capturing names.
#'
#' @inheritParams get_simplify
#' @return capturing names
#' @examples
#' get_named_groups(re2("(a)(?P<name>b)"))
#'
#' regexp = re2("(?P<A>exprA(?P<B>exprB)(?P<C>exprC))((expr5)(?P<D>exprD))")
#'
#' print(regexp)
#' (res = get_named_groups(regexp))
#' re2_match("exprAexprBexprCexpr5exprD", regexp)
#' @export
get_named_groups = function(pattern, ...) {
    cpp_get_named_groups(check_pattern(pattern, ...))
}

#' Get pre-compiled regular expression program size
#'
#' Returns the program size, a very approximate measure of a regexp's "cost".
#' Larger numbers are more expensive than smaller numbers.
#'
#' @inheritParams get_simplify
#' @return a integer
#' @examples
#' get_expression_size(re2("1"))
#' get_expression_size(re2("(1)"))
#' get_expression_size(re2("(?:(?:(?:(?:(?:.)?){100})*)+)"))
#' @export
get_expression_size = function(pattern, ...){
    cpp_get_expression_size(check_pattern(pattern, ...))
}

#' Check NA pattern
#'
#' Returns whether a pre-compiled regular expression is NA.
#'
#' @inheritParams get_simplify
#' @return a boolean
#' @examples
#' is_re2c_na(re2(NA))
#' @export
is_re2c_na = function(pattern, ...){
    cpp_is_re2c_na(check_pattern(pattern, ...))
}

#' Get options of a pre-compiled regular expression
#'
#' Returns options of a pre-compiled regular expression
#'
#' @inheritParams get_simplify
#' @return an list
#' @examples
#' get_options(re2("test"))
#' @export
get_options = function(pattern, ...){
    cpp_get_options(check_pattern(pattern, ...))
}
