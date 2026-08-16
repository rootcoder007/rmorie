% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_cvm_statistic}
\alias{morie_cvm_statistic}
\title{morie_cvm_statistic}
\usage{
morie_cvm_statistic(m, instruments, form = "sum", n_equality = 0L, weights = NULL)
}
\arguments{
\item{m}{Passed to \code{morie_weighted_moments}.}
\item{instruments}{A list; the body reads \code{$instruments} from it.}
\item{form}{Passed to \code{morie_S_function}. Defaults to \code{"sum"}.}
\item{n_equality}{Passed to \code{morie_S_function}. Defaults to \code{0L}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bndsmw_native implementation. Called by \code{morie_confidence_set}. See the file header for the source the module follows.
}
\value{
A list with \code{statistic}, \code{per_instrument}, \code{form}, \code{n_instruments}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_cvm_statistic(D, D)
}
