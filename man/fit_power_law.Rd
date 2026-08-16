% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_power_law}
\alias{fit_power_law}
\title{fit_power_law}
\usage{
fit_power_law(x, y)
}
\arguments{
\item{x}{Passed to \code{.opnclp_vec}.}
\item{y}{Passed to \code{.opnclp_vec}.}
}
\description{
A step of the opnclp_native implementation. Called by \code{compare_scaling}. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{beta}, \code{slope}, \code{r_squared}, \code{range}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
fit_power_law(V, V)
}
