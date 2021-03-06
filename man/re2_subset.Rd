% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset.R
\name{re2_subset}
\alias{re2_subset}
\title{Keep strings matching a pattern.}
\usage{
re2_subset(string, pattern, anchor = 0, omit_na = TRUE,
  parallel = FALSE, grain_size = 1e+05, ...)
}
\arguments{
\item{string}{a character vector}

\item{pattern}{a character vector or pre-compiled regular expressions}

\item{anchor}{see \code{\link{UNANCHORED}}}

\item{omit_na}{omit na result}

\item{parallel}{use multithread}

\item{grain_size}{a minimum chunk size for tuning the behavior of parallel algorithms}

\item{...}{further arguments passed to \code{\link{re2}}}
}
\value{
A character vector.
}
\description{
This is a convenient wrapper around \code{x[re2_detect(x, pattern)]}. Vectorised over strings and patterns.
}
\examples{
fruit <- c("apple", "banana", "pear", "pinapple")
re2_subset(fruit, "a")
re2_subset(fruit, "^a")
re2_subset(fruit, "a$")
re2_subset(fruit, "b")
re2_subset(fruit, "[aeiou]")

re2_subset(c("a", NA, "b"), ".")
}
