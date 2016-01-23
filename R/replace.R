
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
re2_replace = function(pattern, rewrite, input, global = TRUE, ...) UseMethod("re2_replace")

#' @rdname re2_replace
#' @export
re2_replace.re2exp = function(pattern, rewrite, input, global = TRUE, ...){
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
re2_replace.character = function(pattern, rewrite, input, global = TRUE, ...){
    pattern = re2(pattern, ...)
    re2_replace.re2exp(pattern, rewrite, input, global)
}
