#' Show regex pattern in a htmlwidget
#'
#' Show regex pattern in a htmlwidget.
#'
#' @param pattern a pattern string
#' @param width the widget width
#' @param height the widget height
#' @export
show_regex <- function(pattern, width = NULL, height = NULL) {

    # pass the data and settings using 'x'
    x <- list(
        data = pattern
    )

    # create the widget
    htmlwidgets::createWidget("re2r", x, width = width, height = height)
}
