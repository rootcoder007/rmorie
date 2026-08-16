% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Indrv2}
\alias{Indrv2}
\title{Independence of two random variables (Deshmukh eq. 5.3)}
\usage{
Indrv2(joint)
}
\description{
Independence of two random variables (Deshmukh eq. 5.3)
}
\value{
A list with \code{max_deviation}, \code{independent}, \code{margin_row}, \code{margin_col}.
}
\keyword{internal}
\examples{
J <- outer(c(0.3, 0.7), c(0.4, 0.6))
Indrv2(J)
}
