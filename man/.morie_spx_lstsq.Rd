% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_lstsq}
\alias{.morie_spx_lstsq}
\title{.morie_spx_lstsq}
\usage{
.morie_spx_lstsq(A, y, ridge = 0)
}
\arguments{
\item{A}{A matrix; passed to \code{t}.}
\item{y}{Passed to \code{.morie_spx_matvec}.}
\item{ridge}{A flag; the body branches on it. Defaults to \code{0}.}
}
\description{
A step of the sp_fill implementation. Called by \code{.morie_spx_sarneg2}, \code{ShiftInt}, \code{SpErrMod}. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_spx_solve}.
}
\keyword{internal}
