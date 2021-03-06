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



#' Create a pre-compiled regular expression
#'
#' Create a pre-compiled regular expression from a string.
#' @param pattern regular expression pattern
#' @param utf_8 (true)  text and pattern are UTF-8; otherwise Latin-1
#' @param log_error     (false) log syntax and execution errors
#' @param posix_syntax     (false) restrict regexps to POSIX egrep syntax
#' @param longest_match    (false) search for longest match, not first match
#' @param max_mem          (see details)  approx. max memory footprint of RE2
#' @param literal          (false) interpret string as literal, not regexp
#' @param never_nl         (false) never match \\n, even if it is in regexp
#' @param dot_nl           (false) dot matches everything including new line
#' @param never_capture    (false) parse all parens as non-capturing
#' @param case_sensitive   (true)  match is case-sensitive (regexp can override with (?i) unless in posix_syntax mode)
#' @param perl_classes     (false) allow Perl's \\d \\s \\w \\D \\S \\W, when posix_syntax == false this features are always enabled
#' @param word_boundary    (false) allow Perl's \\b \\B (word boundary and not), when posix_syntax == false this features are always enabled
#' @param one_line         (false) ^ and $ only match beginning and end of text, when posix_syntax == false this features are always enabled
#' @param simplify         (true) return a object instead of a list when pattern length is 1.
#' @return a pre-compiled regular expression
#' @details
#' The max_mem option controls how much memory can be used
#' to hold the compiled form of the regexp (the Prog) and
#' its cached DFA graphs.
#'
#' Once a DFA fills its budget, it flushes its cache and starts over.
#' If this happens too often, RE2 falls back on the NFA implementation.
#'
#' For now, make the default budget something close to Code Search.
#'
#' Default maxmem = 8<<20 = 8388608;
#'
#' @examples
#' regexp = re2("test")
#' regexp
#'
#' re2_match("abc\ndef","(?s)(.*)")
#' re2_match("abc\ndef", re2("(?s)(.*)", never_nl = TRUE))
#'
#' re2_detect("\n", re2(".", dot_nl = TRUE))
#' re2_detect("\n", ".")
#'
#' get_number_of_groups(re2("(A)(v)",never_capture = TRUE))
#'
#' re2_match("aaabaaaa",re2("(a|aaa)",longest_match = TRUE))
#' re2_match("aaabaaaa",re2("(a|aaa)",longest_match = FALSE))
#'
#' re2_match("a+b", re2("a+b", literal = TRUE))
#'
#' re2_detect("abc" , re2("abc", posix_syntax = TRUE))
#' re2("(?P<name>re)")
#'
#' \dontrun{
#'
#' expect_error(re2("(?P<name>re)", posix_syntax = TRUE))
#' }
#'
#' @export
re2 = function(pattern,
               utf_8 = TRUE,
               case_sensitive = TRUE,
               posix_syntax = FALSE,
               dot_nl = FALSE,
               literal = FALSE,
               longest_match = FALSE,
               never_nl = FALSE,
               never_capture = FALSE,
               one_line = FALSE,
               perl_classes = FALSE,
               word_boundary = FALSE,
               log_error = FALSE,
               max_mem = 8388608,
               simplify = TRUE) {

    cpp_re2_compile(
        stri_enc_toutf8(pattern),
        log_errors_value = log_error,
        utf_8_value = utf_8,
        case_sensitive_value = case_sensitive,
        posix_syntax_value = posix_syntax,
        dot_nl_value = dot_nl,
        literal_value = literal,
        longest_match_value = longest_match,
        never_nl_value = never_nl,
        never_capture_value = never_capture,
        one_line_value = one_line,
        perl_classes_value = perl_classes,
        word_boundary_value = word_boundary,
        max_mem_value = max_mem,
        simplify_value = simplify

    )
}


#' Escapes all potentially meaningful regexp characters in  'unquoted'.
#'
#' The returned string, used as a regular expression, will exactly match the original string.
#'
#' @param unquoted unquoted string
#' @param parallel multithreading support
#' @param grain_size a minimum chunk size for tuning the behavior of parallel algorithms.
#' @examples
#' quote_meta(c("1.2","abc"))
#' re2_detect("1+2", "1+2")
#' re2_detect("1+2", quote_meta("1+2"))
#' re2_detect("1+2", re2("1+2",literal = TRUE))
#' @return quoted string
#' @export
quote_meta = function(unquoted, parallel = FALSE, grain_size = 100000) {
    res = cpp_quote_meta(stri_enc_toutf8(unquoted), parallel,grain_size)
    res
}
