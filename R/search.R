#' re2_match
#' @param pattern a pre-compiled regular expression or a string
#' @param string a character vector
#' @param result "bool", "value", "object"
#' @param anchor "start": anchor match at the beginning of the string, "both": anchor match at the beginning and the end of the string, "none": no anchor.
#' @param all find all match instead of first match. When all = FALSE and result = "value", the return type will be character vector when there is no capture group or data.frame.
#' @param simplify when result = "value", with only one input string, function will return a data.frame instead of a list of data.frames.
#' @param ... further arguments passed to or from other methods.
#' @export
re2_match = function(pattern, string, result = "bool", anchor = "start", all = TRUE, simplify = TRUE, ...) UseMethod("re2_match")

#' @rdname re2_match
#' @export
re2_match.re2exp = function(pattern, string, result = "bool", anchor = "start", all = TRUE, simplify = TRUE, ...){

}

#' @rdname re2_match
#' @export
re2_match.character = function(pattern, string, result = "bool", anchor = "start", all = TRUE, simplify = TRUE, ...){
    pattern = re2(pattern, ...)
    re2_match.re2exp(pattern, string, result, anchor, all, simplify)
}