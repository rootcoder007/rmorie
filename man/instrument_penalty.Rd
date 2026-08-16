% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{instrument_penalty}
\alias{instrument_penalty}
\title{instrument_penalty}
\usage{
instrument_penalty(g_small, g_large)
}
\arguments{
\item{g_small}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{g_large}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlctmle_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{small}, \code{large}, \code{ratio}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
instrument_penalty(V, V)
}
