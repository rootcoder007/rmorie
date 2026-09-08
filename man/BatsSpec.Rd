% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BatsSpec}
\alias{BatsSpec}
\title{BatsSpec}
\usage{
BatsSpec(periods = numeric(0), harmonics = NULL, use_box_cox = FALSE, 
    use_trend = TRUE, damped = FALSE, p = 0L, q = 0L) 
}
\arguments{
\item{periods}{A vector; its length is taken and its elements indexed. Defaults to \code{numeric(0)}.}
\item{harmonics}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{use_box_cox}{Coerced to logical by the body, with \code{as.logical}. Defaults to \code{FALSE}.}
\item{use_trend}{Coerced to logical by the body, with \code{as.logical}. Defaults to \code{TRUE}.}
\item{damped}{Coerced to logical by the body, with \code{as.logical}. Defaults to \code{FALSE}.}
\item{p}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
\item{q}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
}
\description{
A step of the bats_native implementation. Called by \code{morie_bats}. See the file header for the source the module follows.
}
\value{
The value of \code{structure}.
}
\keyword{internal}
\examples{
BatsSpec()
}
