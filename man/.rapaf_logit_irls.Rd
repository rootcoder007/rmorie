% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rapaf_logit_irls}
\alias{.rapaf_logit_irls}
\title{.rapaf_logit_irls}
\usage{
.rapaf_logit_irls(X, y, ridge = 1e-08, obs_weights = NULL)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
\item{obs_weights}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the rapaf_native implementation. Called by \code{morie_rapaf_rate_ratios_from_logit}. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rapaf_logit_irls(V, V)
}
