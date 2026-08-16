% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{attention_span}
\alias{attention_span}
\title{attention_span}
\usage{
attention_span(weights, position = NULL)
}
\arguments{
\item{weights}{See Usage.}
\item{position}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the sasRec_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mean_lookback}, \code{mass_on_last}, \code{effective_order}, \code{note}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
attention_span(M)
}
