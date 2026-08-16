% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lan_threshold_logit}
\alias{.lan_threshold_logit}
\title{Internal helper: Lan Threshold Logit}
\usage{
.lan_threshold_logit(yk, X)
}
\description{
Internal helper: Lan Threshold Logit
}
\value{
A list with \code{coef}, \code{se}, \code{loglik}, \code{n}, \code{converged}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.lan_threshold_logit(V, D)
}
