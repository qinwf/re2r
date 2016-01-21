#' re2_search_obj
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_search_obj = function(pattern, string, ...) UseMethod("re2_search_obj")

#' re2_search
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_search = function(pattern, string, ...) UseMethod("re2_search")

#' re2_test_search
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_test_search = function(pattern, string, ...) UseMethod("re2_test_search")

#' re2_test_match
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_test_match = function(pattern, string, ...) UseMethod("re2_test_match")

#' re2_match
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_match = function(pattern, string, ...) UseMethod("re2_match")

#' re2_match_obj
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_match_obj = function(pattern, string, ...) UseMethod("re2_match_obj")

#' re2_test_match
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_test_fullmatch = function(pattern, string, ...) UseMethod("re2_test_fullmatch")

#' re2_fullmatch
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_fullmatch = function(pattern, string, ...) UseMethod("re2_fullmatch")

#' re2_fullmatch_obj
#' @param pattern a pre-compiled regular expression or a string
#' @param input a character vector
#' @export
re2_fullmatch_obj = function(pattern, string, ...) UseMethod("re2_fullmatch_obj")