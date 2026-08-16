% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz2_qirls}
\alias{.hrz2_qirls}
\title{Internal helper: fixed-iteration IRLS for the check loss}
\usage{
.hrz2_qirls(X, y, w, tau, niter = 40L, eps = 0.001)
}
\description{
Internal helper: fixed-iteration IRLS for the check loss
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
\examples{
.hrz2_qirls(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8), tau = 0.5)
}
