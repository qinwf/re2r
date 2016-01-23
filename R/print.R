#' print information about a pre-compiled regular expression
#' @param x a pre-compiled regular expression
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2("(.*)@([^.]*)")
#' re2("(?P<name>sd)")
#' @export
print.re2exp = function(x, ...){
    cat("re2 pre-compiled regular expression\n\n")
    cat("pattern: "); cat(get_pattern(x)); cat("\n")
    cat("number of capturing subpatterns: "); cat(get_number_of_groups(x)) ;  cat("\n")
    cat("capturing names with indices: \n"); print(get_named_groups(x)) ;
    cat("expression size: "); cat(get_expression_size(x))
    invisible()
}