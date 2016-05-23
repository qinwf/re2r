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
#' # URL
#'
#' show_regex("((www\\.|(http|https|ftp|news|file)+\\:\\/\\/)[_.a-z0-9-]+\\.[a-z0-9\\/_:@=.+?,##%&~-]*[^.|\\'|\\# |!|\\(|?|,| |>|<|;|\\)])")
#'
#' # major credit cards
#'
#' show_regex("^(?:4[0-9]{12}(?:[0-9]{3})?|5[1-5][0-9]{14}|6011[0-9]{12}|622((12[6-9]|1[3-9][0-9])|([2-8][0-9][0-9])|(9(([0-1][0-9])|(2[0-5]))))[0-9]{10}|64[4-9][0-9]{13}|65[0-9]{14}|3(?:0[0-5]|[68][0-9])[0-9]{11}|3[47][0-9]{13})*$")
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
