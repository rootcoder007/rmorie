% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{positional_encoding}
\alias{positional_encoding}
\title{positional_encoding}
\usage{
positional_encoding(p, L = 10, include_input = TRUE)
}
\arguments{
\item{p}{Passed to \code{.nrfrad_vec}.}
\item{L}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{include_input}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the nrfrad_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
positional_encoding(V)
}
