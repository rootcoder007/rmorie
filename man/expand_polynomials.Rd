% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{expand_polynomials}
\alias{expand_polynomials}
\title{expand_polynomials}
\usage{
expand_polynomials(phi = numeric(0), Phi = numeric(0), theta = numeric(0), Theta = numeric(0), s = 12)
}
\arguments{
\item{phi}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{numeric(0)}.}
\item{Phi}{A vector; its length is taken. Defaults to \code{numeric(0)}.}
\item{theta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{numeric(0)}.}
\item{Theta}{A vector; its length is taken. Defaults to \code{numeric(0)}.}
\item{s}{Passed to \code{.sarima_seasonal_lift}. Defaults to \code{12}.}
}
\description{
A step of the sarima_native implementation. Called by \code{.sarima_fit}, \code{.sarima_package}, \code{.sarimax_fit}. See the file header for the source the module follows.
}
\value{
A list with \code{ar}, \code{ma}.
}
\keyword{internal}
\examples{
expand_polynomials()
}
