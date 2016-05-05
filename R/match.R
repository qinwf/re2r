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
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @param value return value instead of bool result
#' @param anchor a positive number. 0: no anchor. 1: anchor match at the beginning of the string. 2 or larger number: anchor match at the beginning and the end of the string.
#' @param all find all matches instead of the first match. When value is true, a matched character matrix or a list will be returned.
#' @param tolist return a list instead of matrix when all and value is TRUE.
#' @param parallel multithreading support
#' @param ... further arguments passed to or from other methods.
#' @examples
#'
#' test_string = "this is just one test";
#' re2_match(test_string, "(o.e)")
#'
#' (res = re2_match(test_string, "(o.e)"))
#' str(res)
#'
#' (res = re2_match(test_string, "(?P<testname>this)( is)"))
#' str(res)
#'
#' (res = re2_match_all(test_string, "(is)"))
#'
#' test_string = c("this is just one test", "the second test");
#' (res = re2_match_all(test_string, "(is)"))
#'
#' test_string = c("this is just one test", "the second test")
#' (res = re2_match(test_string, "is"))
#'
#' regexp = re2("test",case_sensitive = FALSE)
#' re2_match("TEST", regexp)
#'
#' re2_match_list(c("   aaa b!@#$@#$cccc","   aaa bb cccc"),
#'  "\\s*(\\w+)")
#' @export
re2_match = function(input,
                     pattern,
                     anchor = 0,
                     value = TRUE,
                     all = FALSE,
                     tolist = FALSE,
                     parallel = FALSE,
                     ...) {
    if (is.character(pattern)) {
        pattern = re2(pattern, ...)
    }
    cpp_match(stri_enc_toutf8(input), pattern,   value, anchor, all, tolist, parallel)
}

#' Test a pattern in strings, and return boolean.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @param anchor a positive number. 0: no anchor. 1: anchor match at the beginning of the string. 2 or larger number: anchor match at the beginning and the end of the string.
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_detect("one", "(o.e)")
#' @export
re2_detect = function(input,
                      pattern,
                      anchor = 0,
                      ...) {
    re2_match(input, pattern, anchor, FALSE, FALSE, FALSE, FALSE, ...)
}


#' Test a pattern in strings with multithread, and return boolean.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @param anchor a positive number. 0: no anchor. 1: anchor match at the beginning of the string. 2 or larger number: anchor match at the beginning and the end of the string.
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_pdetect("one", "(o.e)")
#' @export
re2_pdetect = function(input,
                      pattern,
                      anchor = 0,
                      ...) {
    re2_match(input, pattern, anchor, FALSE, FALSE, FALSE, TRUE, ...)
}

#' Find matched groups from strings with multithread.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @param anchor a positive number. 0: no anchor. 1: anchor match at the beginning of the string. 2 or larger number: anchor match at the beginning and the end of the string.
#' @param ... further arguments passed to or from other methods.
#' @examples
#'
#' test_string = "this is just one test";
#' re2_pdetect(test_string, "(o.e)")
#'
#' (res = re2_pmatch(test_string, "(o.e)"))
#' str(res)
#'
#' (res = re2_pmatch(test_string, "(?P<testname>this)( is)"))
#' str(res)
#'
#' (res = re2_pmatch_all(test_string, "(is)"))
#'
#' test_string = c("this is just one test", "the second test");
#' (res = re2_pmatch_all(test_string, "(is)"))
#'
#' test_string = c("this is just one test", "the second test")
#' (res = re2_pmatch_all(test_string, "is"))
#'
#' regexp = re2("test",case_sensitive = FALSE)
#' re2_pmatch("TEST", regexp)
#'
#' re2_pmatch_list(c("   aaa b!@#$@#$cccc","   aaa bb cccc"),
#'  "\\s*(\\w+)", anchor = 1)
#' @export
re2_pmatch = function(input,
                       pattern,
                       anchor = 0,
                       ...) {
    re2_match(input, pattern, anchor, TRUE, FALSE, FALSE, TRUE, ...)
}

#' @export
#' @rdname re2_match
re2_match_all = function(input,
                      pattern,
                      anchor = 0,
                      ...) {
    re2_match(input, pattern, anchor, TRUE, TRUE, FALSE, FALSE, ...)
}

#' @export
#' @rdname re2_match
re2_match_list = function(input,
                           pattern,
                           anchor = 0,
                           ...) {
    re2_match(input, pattern, anchor,  TRUE, TRUE, TRUE, FALSE, ...)
}

#' @export
#' @rdname re2_pmatch
re2_pmatch_all = function(input,
                           pattern,
                           anchor = 0,
                           ...) {
    re2_match(input, pattern, anchor, TRUE, TRUE, FALSE, TRUE, ...)
}

#' @export
#' @rdname re2_pmatch
re2_pmatch_list = function(input,
                            pattern,
                            anchor = 0,
                            ...) {
    re2_match(input, pattern, anchor, TRUE, TRUE, TRUE, TRUE, ...)
}
