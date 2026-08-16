% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpztpmle}
\alias{.gpztpmle}
\title{.gpztpmle}
\usage{
.gpztpmle(y_positive, tol = 1e-12, max_iter = 200)
}
\arguments{
\item{y_positive}{Passed to \code{.gpflat}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-12}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{.gpzapbestsplit}, \code{Msm325}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gpztpmle(V)
}
