% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{inject_special_codes}
\alias{inject_special_codes}
\title{inject_special_codes}
\usage{
inject_special_codes(x, rate = 0.02, codes = c(97L, 98L, 99L, 997L, 998L, 999L))
}
\arguments{
\item{x}{See Usage.}
\item{rate}{Defaults to \code{0.02}.}
\item{codes}{Defaults to \code{c(97L, 98L, 99L, 997L, 998L, 999L)}.}
}
\description{
Part of the synthetic implementation; see the file header for the source it follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
inject_special_codes(V)
}
