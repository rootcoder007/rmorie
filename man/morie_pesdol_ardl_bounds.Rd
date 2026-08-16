% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pesdol_ardl_bounds}
\alias{morie_pesdol_ardl_bounds}
\title{morie_pesdol_ardl_bounds}
\usage{
morie_pesdol_ardl_bounds(y, x, p = 1, q = 1)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x}{A matrix; passed to \code{as.matrix}.}
\item{p}{Numeric; passed to \code{max}. Defaults to \code{1}.}
\item{q}{Numeric; passed to \code{max}. Defaults to \code{1}.}
}
\description{
A step of the pesdol_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{long_run}, \code{coefficients}, \code{residuals}, \code{fitted}, \code{speed_of_adjustment}, \code{f_statistic}, \code{bound_lower}, \code{bound_upper}, \code{verdict}, \code{n_used}, \code{n_params}, \code{rss_unrestricted}, \code{rss_restricted}, \code{p}, \code{q}, \code{n_regressors}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_pesdol_ardl_bounds(V, V)
}
