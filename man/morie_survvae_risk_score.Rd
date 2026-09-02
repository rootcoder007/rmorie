% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_risk_score}
\alias{morie_survvae_risk_score}
\title{Risk at a horizon: \eqn{1 - S(t | x)}}
\usage{
morie_survvae_risk_score(fit_result, X, horizon = NULL) 
}
\arguments{
\item{X}{List of covariate vectors.}
\item{horizon}{Optional numeric horizon; if \code{NULL} the   median observed time is used.}

\item{fit_result}{Argument \code{fit_result}; see Usage.}
}
\description{
Default horizon is the median observation time -- the \code{len // 2}-th element of the sorted \code{times} field of the fit, matching the Python arm.
}
\value{
Numeric vector of risks, one per row of \code{X}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_survvae_risk_score(D, V)
}
