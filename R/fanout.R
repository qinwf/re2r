#' Get program fanout
#' Outputs the program fanout as a histogram bucketed by powers of 2.
#' Returns the number of the largest non-empty bucket.
#' @param regexp a pre-compiled regular expression
#' @export
get_program_fanout = function(regexp){
    res = cpp_get_program_fanout(regexp)
    return(data.frame(
        index = as.numeric(names(res)),
        value = as.numeric(res))
        )
}
