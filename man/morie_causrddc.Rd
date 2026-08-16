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
\item{y}{A vector; its length is taken.}
\item{x}{A vector; its length is taken and its elements indexed.}
\item{treatment}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{cutoff}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{nu}{Passed to \code{morie_causrddc_rd_bandwidth}. Defaults to \code{0}.}
\item{p}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{q}{Optional; may be \code{NULL}. Passed to \code{morie_causrddc_rd_bandwidth}.}
\item{h}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{b}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{kernel}{Passed to \code{morie_causrddc_rd_bandwidth}. Defaults to \code{"triangular"}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.05}.}
\item{vce}{One of \code{"hc"}, \code{"nn"}. Defaults to \code{"nn"}.}
\item{J}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
}
\description{
A step of the causrddc_native implementation. Called by \code{morie_rdrobu}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{bias_corrected}, \code{se_conventional}, \code{se_robust}, \code{ci_conventional}, \code{ci_bias_corrected}, \code{ci_robust}, \code{pvalue_robust}, \code{h}, \code{b}, \code{rho}, \code{p}, \code{q}, \code{nu}, \code{kernel}, \code{vce}, \code{alpha}, \code{n}, \code{n_left}, \code{n_right}, \code{weights_conventional}, \code{weights_bias_corrected}, \code{fuzzy}, \code{method}.
}
\keyword{internal}
