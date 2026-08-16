% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gms_critical_value}
\alias{morie_gms_critical_value}
\title{morie_gms_critical_value}
\usage{
morie_gms_critical_value(m, instruments, form = "sum", n_equality = 0L, level = 0.95, reps = 200L, seed = 0, kappa = NULL)
}
\arguments{
\item{m}{See Usage.}
\item{instruments}{See Usage.}
\item{form}{Defaults to \code{"sum"}.}
\item{n_equality}{Defaults to \code{0L}.}
\item{level}{Defaults to \code{0.95}.}
\item{reps}{Defaults to \code{200L}.}
\item{seed}{Defaults to \code{0}.}
\item{kappa}{Defaults to \code{NULL}.}
}
\description{
Part of the bndsmw_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{critical_value}, \code{kappa}, \code{reps}, \code{level}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_gms_critical_value(D, D)
}
