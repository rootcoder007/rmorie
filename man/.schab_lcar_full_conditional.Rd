% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_lcar_full_conditional}
\alias{.schab_lcar_full_conditional}
\title{.schab_lcar_full_conditional}
\usage{
.schab_lcar_full_conditional(u, adjacency, rho, sigma2 = 1)
}
\arguments{
\item{u}{A matrix; passed to \code{\%*\%}.}
\item{adjacency}{A matrix; passed to \code{as.matrix}.}
\item{rho}{Numeric; combined arithmetically in the body.}
\item{sigma2}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the schab_glmm_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{variance}, \code{n_neighbours}.
}
\keyword{internal}
