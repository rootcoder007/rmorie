% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{density_is_view_independent}
\alias{density_is_view_independent}
\title{density_is_view_independent}
\usage{
density_is_view_independent(model, point, directions, tol = 1e-09)
}
\arguments{
\item{model}{Accepted by the signature and not used anywhere in the body.}
\item{point}{Passed to \code{.nrfrad_vec}.}
\item{directions}{Iterated over elementwise, with \code{vapply}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.}
}
\description{
A step of the nrfrad_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{sigmas}, \code{max_deviation}, \code{view_independent}, \code{note}.
}
\keyword{internal}
\examples{
model <- function(p, d) list(sigma = sum(p^2), rgb = c(0.5, 0.5, 0.5))
r <- density_is_view_independent(model, point = c(1, 2, 3),
                                 directions = list(c(1, 0, 0), c(0, 1, 0),
                                                   c(0, 0, 1)))
r$max_deviation < 1e-9
}
