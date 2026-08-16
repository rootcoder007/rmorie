% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ps_keep}
\alias{.mor_ps_keep}
\title{LOUD WARNING, because this is not a numerical detail: discarding}
\usage{
.mor_ps_keep(ps, trim = c(0.1, 0.9), trim_type = "value")
}
\arguments{
\item{ps}{A vector; its length is taken.}
\item{trim}{Optional; may be \code{NULL}. A vector; indexed elementwise. Defaults to \code{c(0.1, 0.9)}.}
\item{trim_type}{Defaults to \code{"value"}.}
}
\description{
CHANGES THE ESTIMAND.  The result is no longer the ATE on the whole sample but the ATE on the retained subpopulation -- which is the entire point of their Sec. 5, since that subpopulation is the one the data can actually identify.  The estimators report n_discarded and an estimand note whenever this route is taken.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_ps_keep(V)
}
