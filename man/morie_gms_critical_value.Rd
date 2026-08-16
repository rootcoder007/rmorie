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
\item{m}{A matrix; passed to \code{as.matrix}.}
\item{instruments}{A list; the body reads \code{$instruments} from it.}
\item{form}{Passed to \code{morie_S_function}. Defaults to \code{"sum"}.}
\item{n_equality}{Passed to \code{morie_S_function}. Defaults to \code{0L}.}
\item{level}{Numeric; combined arithmetically in the body. Defaults to \code{0.95}.}
\item{reps}{Defaults to \code{200L}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{kappa}{Defaults to \code{NULL}.}
}
\description{
A step of the bndsmw_native implementation. Called by \code{morie_confidence_set}. See the file header for the source the module follows.
}
\value{
A list with \code{critical_value}, \code{kappa}, \code{reps}, \code{level}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_gms_critical_value(D, D)
}
