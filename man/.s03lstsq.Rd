% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03lstsq}
\alias{.s03lstsq}
\title{.s03lstsq}
\usage{
.s03lstsq(X, y, ridge = 1e-10)
}
\arguments{
\item{X}{A matrix; passed to \code{t}.}
\item{y}{Passed to \code{.s03matvec}.}
\item{ridge}{Passed to \code{.s03ridgesolve}. Defaults to \code{1e-10}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.btnpqr_fit}, \code{.btsieve_arfit}, \code{.dssoot_ols} and 36 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{.s03ridgesolve}.
}
\keyword{internal}
