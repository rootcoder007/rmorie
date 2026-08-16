% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funBoot}
\alias{morie_funBoot}
\title{morie_funBoot}
\usage{
morie_funBoot(curves, statistic = NULL, alpha = 0.05, B = 500L, metric = "l2", smooth = 0, seed = 0)
}
\arguments{
\item{curves}{A matrix; indexed by row and column.}
\item{statistic}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{B}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{500L}.}
\item{metric}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"l2"}.}
\item{smooth}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
Part of the funBoot_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{center}, \code{radius}, \code{estimate}, \code{distances}, \code{n_within}, \code{metric}, \code{alpha}, \code{B}, \code{seed}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_funBoot(D)
}
