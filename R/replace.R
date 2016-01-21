re2_replace_core = function(pattern, rewrite, input, global){
    if (check_windows_strings(input)) input = enc2utf8(input)
    if (check_windows_strings(rewrite))  rewrite = enc2utf8(rewrite)

    res = cpp_replace(pattern, rewrite, input, global)

    if (update_windows_strings()) {
        Encoding(res) = "UTF-8"
    }
    return(res)
}

#' Replace matched patterns in a string.
#'
#' Replace the first match of "pattern" in "str" with "rewrite".
#' Within "rewrite", backslash-escaped digits (\\1 to \\9) can be
#' used to insert text matching corresponding parenthesized group
#' from the pattern.  \\0 in "rewrite" refers to the entire matching
#' text.
#'
#' @param pattern a pre-compiled regular expression or a string
#' @param rewrite replace the first match of "pattern" in "input" with "rewrite"
#' @param input a character vector
#' @return a character vector
#' @examples
#' regexp = re2_compile("b+")
#' re2_replace(regexp,"d", "yabba dabba doo") == "yada dabba doo"
#' re2_replace("b+","d", "yabba dabba doo") == "yada dabba doo"
#' @export
re2_replace = function(pattern, rewrite, input, ...) UseMethod("re2_replace")

#' @rdname re2_replace
#' @export
re2_replace.re2exp = function(pattern, rewrite, input, ...){
    re2_replace_core(pattern, rewrite, input, FALSE)
}

#' @rdname re2_replace
#' @export
re2_replace.character = function(pattern, rewrite, input, ...){
    pattern = re2_compile(pattern)
    re2_replace_core(pattern, rewrite, input, FALSE)
}


#' Replaces successive non-overlapping occurrences
#'
#' Like re2_replace, except replaces successive non-overlapping occurrences
#' of the pattern in the string with the rewrite.
#'
#' Within "rewrite", backslash-escaped digits (\\1 to \\9) can be
#' used to insert text matching corresponding parenthesized group
#' from the pattern.  \\0 in "rewrite" refers to the entire matching
#' text.
#' @param pattern a pre-compiled regular expression or a string
#' @param rewrite replace the first match of "pattern" in "input" with "rewrite"
#' @param input a character vector
#' @return a character vector
#' @examples
#' regexp = re2_compile("b+")
#' re2_replace_all(regexp,"d", "yabba dabba doo") == "yada dada doo"
#' re2_replace_all("b+","d", "yabba dabba doo") == "yada dada doo"
#' @export
re2_replace_all = function(pattern, rewrite, input, ...) UseMethod("re2_replace_all")

#' @rdname re2_replace_all
#' @export
re2_replace_all.re2exp = function(pattern, rewrite, input, ...){
    re2_replace_core(pattern, rewrite, input, TRUE)
}

#' @rdname re2_replace_all
#' @export
re2_replace_all.character = function(pattern, rewrite, input, ...){
    pattern = re2_compile(pattern)
    re2_replace_core(pattern, rewrite, input, TRUE)
}
