% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ks_critical_value}
\alias{ks_critical_value}
\title{ks_critical_value}
\usage{
ks_critical_value(m, instruments, form = "sum", n_equality = 0L, level = 0.95, reps = 200L, seed = 0L, kappa = NULL)
}
\arguments{
\item{m}{A matrix; the body checks with \code{is.matrix}.}
\item{instruments}{A list; the body reads \code{$instruments} from it.}
\item{form}{Passed to \code{S_function}. Defaults to \code{"sum"}.}
\item{n_equality}{Passed to \code{S_function}. Defaults to \code{0L}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
\item{reps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
\item{kappa}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bnskmt_native implementation. Called by \code{ks_confidence_set}. See the file header for the source the module follows.
}
\value{
A list with \code{critical_value}, \code{kappa}, \code{reps}, \code{level}.
}
\keyword{internal}
\examples{
set.seed(2)
X <- matrix(runif(60), 30, 2)
inst <- hypercube_instruments(X, n_levels = 2L)
r <- ks_critical_value(X[, 1] - 0.4, inst)
str(r, max.level = 1)
}
