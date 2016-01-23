
#' Extract one matched patterns in a string.
#'
#' Like Replace, except that if the pattern matches, "rewrite"
#' is copied into "out" with substitutions.  The non-matching
#' portions of "text" are ignored.
#'
#' Returns true iff a match occurred and the extraction happened
#' @param pattern a pre-compiled regular expression or a string
#' @param rewrite replace the first match of "pattern" in "input" with "rewrite"
#' @param input a character vector
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2_extract("(.)","yabba dabba doo")
#' re2_extract("(.*)@([^.]*)","test@me.com","\\2!\\1")
#' @export
re2_extract = function(pattern, input, rewrite = "\\1", ...) UseMethod("re2_extract")

#' @rdname re2_extract
#' @export
re2_extract.re2exp = function(pattern, input, rewrite = "\\1", ...){
    if (check_windows_strings(input)) input = enc2utf8(input)
    if (check_windows_strings(rewrite))  rewrite = enc2utf8(rewrite)

    res = cpp_extract(pattern, rewrite, input)

    if (update_windows_strings()) {
        Encoding(res) = "UTF-8"
    }
    return(res)
}

#' @rdname re2_extract
#' @export
re2_extract.character = function(pattern, input, rewrite = "\\1", ...){
    pattern = re2(pattern, ...)
    re2_extract.re2exp(pattern, input, rewrite)
}
