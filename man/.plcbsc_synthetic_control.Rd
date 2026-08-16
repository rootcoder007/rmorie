% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.plcbsc_synthetic_control}
\alias{.plcbsc_synthetic_control}
\title{.plcbsc_synthetic_control}
\usage{
.plcbsc_synthetic_control(x_treated, x_donors, v = NULL, max_iter = 5000, tol = 1e-12, step = NULL)
}
\arguments{
\item{x_treated}{See Usage.}
\item{x_donors}{See Usage.}
\item{v}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{5000}.}
\item{tol}{Defaults to \code{1e-12}.}
\item{step}{Defaults to \code{NULL}.}
}
\description{
Part of the plcbsc_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{weights}, \code{loss}, \code{fitted}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
