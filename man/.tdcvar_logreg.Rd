% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tdcvar_logreg}
\alias{.tdcvar_logreg}
\title{.tdcvar_logreg}
\usage{
.tdcvar_logreg(X, y, max_iter = 25L, tol = 1e-08)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{max_iter}{Defaults to \code{25L}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
Part of the tdcvar_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\references{
----------
Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC. Ch. 20 (treatment-confounder feedback),
Sec. 21.2 (IP weighting for time-varying treatments), Sec. 12.3 (the
stabilized weights and the mean-1 diagnostic).

Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period", Mathematical Modelling
7(9-12), 1393-1512, doi:10.1016/0270-0255(86)90088-6 -- where the
problem and the weighted solution were first set out.
}
\keyword{internal}
