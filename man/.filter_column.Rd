% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.filter_column}
\alias{.filter_column}
\title{.filter_column}
\usage{
.filter_column(w, ar, ma)
}
\arguments{
\item{w}{A vector; its length is taken and its elements indexed.}
\item{ar}{Passed to \code{.sarima_state_space}.}
\item{ma}{Passed to \code{.sarima_state_space}.}
}
\description{
A step of the sarimax_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{v}, \code{f}.
}
\keyword{internal}
\examples{
.filter_column(w = c(1, 2, 3, 4, 5, 6, 7, 8), ar = c(1, 2, 3, 4, 5, 6, 7, 8), ma = c(1, 2, 3, 4, 5, 6, 7, 8))
}
