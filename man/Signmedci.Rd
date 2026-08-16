% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signmedci}
\alias{Signmedci}
\title{Median CI from sign-test inversion -- eq. (5.4.11), p. 179}
\usage{
Signmedci(x, alpha = 0.05)
}
\description{
Median CI from sign-test inversion -- eq. (5.4.11), p. 179
}
\value{
A list with \code{lower}, \code{upper}, \code{r}, \code{s}, \code{coverage}, \code{tail}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Signmedci(V)
}
