% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LsiSerH}
\alias{LsiSerH}
\title{Eq (3.45): cascading two LSI systems convolves their impulse}
\usage{
LsiSerH(h_1, h_2, n = NULL)
}
\arguments{
\item{h_1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h_2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
responses.  Convolution commutes, so a filter chain may be reordered, and the result is len(h1) + len(h2) - 1 long.
}
\value{
A list with \code{h}, \code{n_taps}, \code{value}, \code{index}, \code{commutes}, \code{longer_than_either_input}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
LsiSerH(V, V)
}
