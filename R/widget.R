remove_named = function(string){
    gsub("(?<!\\\\)\\?P(?<!\\\\)\\<[^>]+(?<!\\\\)\\>",string,replacement = "", perl = T)
}

#' Show regex pattern in a htmlwidget
#'
#' Show regex pattern in a htmlwidget.
#'
#' @param pattern a pattern string
#' @param width the widget width
#' @param height the widget height
#' @export
show_regex <- function(pattern, width = NULL, height = NULL) {

    if (is.character(pattern)){
        res = re2(pattern)
    }else if (inherits(pattern,"re2c")) {
        res = pattern
    } else if (is.list(pattern)){
        return(show_regex(pattern[[1]], width, height))
    } else {
        stop("pattern is a string or a pre-compiled regular expression.")
    }

    res = remove_named(get_pattern(res))

    # pass the data and settings using 'x'
    x <- list(
        data = res
    )

    # create the widget
    htmlwidgets::createWidget("re2r", x, width = width, height = height)
}
