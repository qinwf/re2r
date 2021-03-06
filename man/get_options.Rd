% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attr.R
\name{get_options}
\alias{get_options}
\title{Get options of a pre-compiled regular expression}
\usage{
get_options(pattern, ...)
}
\arguments{
\item{pattern}{a pre-compiled regular expression or a string}

\item{...}{further arguments passed to \code{\link{re2}}}
}
\value{
an list
}
\description{
Returns options of a pre-compiled regular expression
}
\examples{
get_options(re2("test"))
}
