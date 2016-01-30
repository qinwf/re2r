#' Create a pre-compiled regular expression
#'
#' @param varible varible name
#' @param pattern regular expression pattern
#' @examples
#' regexp %<~% "(12)"
#' regexp
#' @seealso more options with \code{\link{re2}}
#' @export
`%<~%` = function(varible, pattern){
    parent <- parent.frame()
    rhss  <- substitute(pattern) # the right-hand sides
    lhs   <- substitute(varible) # the left-hand side.
    eval(call("<-", lhs, re2(rhss)), parent, parent)
}

#' check a regular expression
#'
#' return re2_match(pattern, string)
#'
#' @param string a character vector
#' @param pattern a pre-compiled regular expression or a string
#' @examples
#' "sd" %=~% c("pt","sd")
#' "sd" %!~% c("pt","sd")
#' @export
`%=~%` = function(pattern, string){
    re2_match(pattern, string, value = FALSE, anchor = "none")
}

#' @rdname `%=~%`
#' @export
`%!~%` = function(pattern, string){
    !re2_match(pattern, string, value = FALSE, anchor = "none")
}
