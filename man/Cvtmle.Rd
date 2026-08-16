% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Cvtmle}
\alias{Cvtmle}
\title{Cross-validated TMLE of the ATE (Zheng & van der Laan 2011)}
\usage{
Cvtmle(y, a, q0, q1, g, fold, n_newton = 50)
}
\description{
Cross-validated TMLE of the ATE (Zheng & van der Laan 2011)
}
\value{
A list with \code{estimate}, \code{se}, \code{psi_fold}, \code{eps_fold}, \code{n_folds}, \code{n}.
}
\keyword{internal}
