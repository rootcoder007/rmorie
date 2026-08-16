% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mfovsm_logreg_fit}
\alias{.mfovsm_logreg_fit}
\title{.mfovsm_logreg_fit}
\usage{
.mfovsm_logreg_fit(y, X, max_iter = 25, tol = 1e-08)
}
\arguments{
\item{y}{See Usage.}
\item{X}{See Usage.}
\item{max_iter}{Defaults to \code{25}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
Part of the mfovsm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{coef}, \code{fitted}.
}
\references{
----------
Robins, J. M. & Hernan, M. A. (2009) "Estimation of the causal effects
of time-varying exposures", in Fitzmaurice, G., Davidian, M.,
Verbeke, G. & Molenberghs, G. (eds.), Longitudinal Data Analysis,
Chapman & Hall/CRC Handbooks of Modern Statistical Methods, 553-599,
doi:10.1201/9781420011579.ch23.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
Chapman & Hall/CRC, Sec. 12.5 (effect modification and marginal
structural models) and Sec. 21.2 (the time-varying weights).
}
\keyword{internal}
