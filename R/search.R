#' re2_match
#' @param pattern a pre-compiled regular expression or a string
#' @param string a character vector
#' @param result bool, value or object
#' @param anchor "match": anchor match at the beginning of the string, "fullmatch": anchor match at the beginning and the end of the string, "search": no anchor.
#' @param ... further arguments passed to or from other methods.
#' @export
re2_match = function(pattern, string, result, anchor, ...) UseMethod("re2_match")

#' @rdname re2_match
#' @export
re2_match.re2exp = function(pattern, string, result, anchor, ...){

}

#' @rdname re2_match
#' @export
re2_match.character = function(pattern, string, result, anchor, ...){
    pattern = re2(pattern, ...)
    re2_match.re2exp(pattern, string, result, anchor)
}