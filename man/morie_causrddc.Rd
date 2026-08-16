% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causrddc}
\alias{morie_causrddc}
\title{morie_causrddc}
\usage{
morie_causrddc(y, x, treatment = NULL, cutoff = 0, nu = 0, p = 1, q = NULL, h = NULL, b = NULL, kernel = "triangular", alpha = 0.05, vce = "nn", J = 3)
}
\arguments{
\item{y}{See Usage.}
\item{x}{See Usage.}
\item{treatment}{Defaults to \code{NULL}.}
\item{cutoff}{Defaults to \code{0}.}
\item{nu}{Defaults to \code{0}.}
\item{p}{Defaults to \code{1}.}
\item{q}{Defaults to \code{NULL}.}
\item{h}{Defaults to \code{NULL}.}
\item{b}{Defaults to \code{NULL}.}
\item{kernel}{Defaults to \code{"triangular"}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{vce}{Defaults to \code{"nn"}.}
\item{J}{Defaults to \code{3}.}
}
\description{
Part of the causrddc_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{bias_corrected}, \code{se_conventional}, \code{se_robust}, \code{ci_conventional}, \code{ci_bias_corrected}, \code{ci_robust}, \code{pvalue_robust}, \code{h}, \code{b}, \code{rho}, \code{p}, \code{q}, \code{nu}, \code{kernel}, \code{vce}, \code{alpha}, \code{n}, \code{n_left}, \code{n_right}, \code{weights_conventional}, \code{weights_bias_corrected}, \code{fuzzy}, \code{method}.
}
\keyword{internal}
