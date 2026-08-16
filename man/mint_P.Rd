% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mint_P}
\alias{mint_P}
\title{mint_P}
\usage{
mint_P(S, W = NULL, method = "shrink", residuals = NULL, ridge = 1e-10)
}
\arguments{
\item{S}{A matrix; indexed by row and column.}
\item{W}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{method}{One of \code{"custom"}, \code{"ols"}, \code{"shrink"}, \code{"wls"}. Defaults to \code{"shrink"}.}
\item{residuals}{Optional; may be \code{NULL}. A matrix; passed to \code{nrow}.}
\item{ridge}{A matrix; passed to \code{diag}. Defaults to \code{1e-10}.}
}
\description{
A step of the hierF_native implementation. Called by \code{mint_reconcile}. See the file header for the source the module follows.
}
\value{
A list with \code{P}, \code{lambda}.
}
\keyword{internal}
