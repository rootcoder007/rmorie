% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crfflt_cf_filter}
\alias{morie_crfflt_cf_filter}
\title{Extract the band by one of three routes}
\usage{
morie_crfflt_cf_filter(x, p_low = 6, p_high = 32, method = "asymmetric", p = NULL, drift = TRUE) 
}
\arguments{
\item{x}{Numeric vector, the series.}
\item{p_low}{Numeric, lower period bound.}
\item{p_high}{Numeric, upper period bound.}
\item{method}{One of \code{"asymmetric"}, \code{"symmetric"} or   \code{"one_sided"}.}
\item{p}{Integer, leads-and-lags count for the symmetric route.}
\item{drift}{Logical, drift-adjust the data first.}
}
\description{
Extract the band by one of three routes
}
\value{
A list mirroring the Python arm\'s payload.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_crfflt_cf_filter(V)
}
