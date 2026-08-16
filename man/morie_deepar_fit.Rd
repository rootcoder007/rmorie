% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_deepar_fit}
\alias{morie_deepar_fit}
\title{Fit the conditional mean by a scaled autoregression}
\usage{
morie_deepar_fit(z, n_lags = 2L, likelihood = "negative-binomial", ridge = 1e-06)
}
\arguments{
\item{z}{Numeric vector.}
\item{n_lags}{Integer number of lags.}
\item{likelihood}{One of \code{"negative-binomial"} or   \code{"gaussian"}.}
\item{ridge}{Numeric ridge penalty for the least-squares fit.}
}
\description{
A linear autoregression stands in for the recurrent network: the scaling, the likelihood and the sampling are what this module is about, and they are identical either way.
}
\value{
A list with the fitted parameters and the same field   names as the Python arm.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_deepar_fit(V)
}
