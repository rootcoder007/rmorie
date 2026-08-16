% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_olsfit}
\alias{.t4_olsfit}
\title{.t4_olsfit}
\usage{
.t4_olsfit(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{y}{A matrix; passed to \code{crossprod}.}
}
\description{
A step of the tail4_core implementation. Called by \code{.t4_poly4}, \code{.t4_wls_int}, \code{Lctest} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{fitted}, \code{resid}, \code{xtxinv}.
}
\keyword{internal}
