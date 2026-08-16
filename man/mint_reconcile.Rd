% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mint_reconcile}
\alias{mint_reconcile}
\title{mint_reconcile}
\usage{
mint_reconcile(base, S, method = "shrink", residuals = NULL, W = NULL, ridge = 1e-10)
}
\arguments{
\item{base}{See Usage.}
\item{S}{See Usage.}
\item{method}{Defaults to \code{"shrink"}.}
\item{residuals}{Defaults to \code{NULL}.}
\item{W}{Defaults to \code{NULL}.}
\item{ridge}{Defaults to \code{1e-10}.}
}
\description{
Part of the hierF_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{reconciled}, \code{bottom}, \code{base}, \code{P}, \code{S}, \code{method}, \code{shrinkage}, \code{n_series}, \code{n_bottom}, \code{coherent}, \code{ps_identity_error}, \code{adjustment}, \code{cite}, \code{method_detail}.
}
\keyword{internal}
