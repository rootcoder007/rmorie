% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{reference_free}
\alias{reference_free}
\title{reference_free}
\usage{
reference_free(hyp, src, W, b = NULL)
}
\arguments{
\item{hyp}{Passed to \code{.comet_vec}.}
\item{src}{Passed to \code{.comet_vec}.}
\item{W}{A matrix; passed to \code{nrow}.}
\item{b}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the comet_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{score}, \code{reference_used}, \code{note}.
}
\keyword{internal}
