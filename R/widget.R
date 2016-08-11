remove_named = function(string){
    gsub("(?<!\\\\)\\?P(?<!\\\\)\\<[^>]+(?<!\\\\)\\>",string,replacement = "", perl = T)
}

#' Show regex pattern in a htmlwidget
#'
#' Show JS-style regex pattern in an htmlwidget.
#'
#' Most parts of RE2 regex syntax are supported, except for some special Unicode character classes.
#'
#' @param pattern a pattern string
#' @param width the widget width
#' @param height the widget height
#' @seealso https://regexper.com/documentation.html
#' @examples
#' # Skip on CRAN
#'
#' \dontrun{
#'
#' # US ZIP code
#'
#' show_regex("[0-9]{5}(?:-[0-9]{4})?")
#'
#' # Email
#'
#' show_regex("\\b[a-zA-Z0-9._%-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}\\b")
#'
#' # Hex value
#'
#' show_regex("#?([a-f0-9]{6}|[a-f0-9]{3})")
#'
#' }
#'
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
