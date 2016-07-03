#' Anchors for pattern
#'
#' Anchors for regular expression pattern string.
#'
#' UNANCHORED - No anchoring
#'
#' ANCHOR_START - Anchor at start only
#'
#' ANCHOR_BOTH - Anchor at start and end
#' @examples
#' re2_match("This is an apple.", "(is)",anchor = ANCHOR_BOTH)
#' re2_match("This is an apple.", "(This)",anchor = ANCHOR_START)
#' re2_match("This is an apple.", "(is)",anchor = UNANCHORED)
#' @export
UNANCHORED = 0

#' @rdname UNANCHORED
#' @export
ANCHOR_START = 1

#' @rdname UNANCHORED
#' @export
ANCHOR_BOTH = 2
