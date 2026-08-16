% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_semivariogram}
\alias{.sp_semivariogram}
\title{Internal: semivariogram gamma(h) = c0 + sigma0^2 (1 - R(h)), gamma(0) = 0.}
\usage{
.sp_semivariogram(h, nugget, sill, rng, model)
}
\arguments{
\item{h}{Numeric vector of non-negative lags.}
\item{nugget}{Nugget c0, non-negative.}
\item{sill}{Partial sill sigma0^2, non-negative.}
\item{rng}{Range, positive.}
\item{model}{Correlogram family.}
}
\description{
The nugget is a discontinuity AT the origin (Sec 4.3.6, p. 150).
}
\value{
Numeric vector gamma(h).
}
\keyword{internal}
\examples{
lags <- (1:12) * 0.5
.sp_semivariogram(lags, 0.3, 2, 6, "exponential")
}
