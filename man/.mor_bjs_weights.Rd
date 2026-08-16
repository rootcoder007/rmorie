% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_bjs_weights}
\alias{.mor_bjs_weights}
\title{Exact linear weights v with sum(v * Y) == the estimate, used for the}
\usage{
.mor_bjs_weights(W, treated, Xc, max_iter = 2000L, tol = 1e-13)
}
\arguments{
\item{W}{A matrix; passed to \code{nrow}.}
\item{treated}{A flag; the body branches on it.}
\item{Xc}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{max_iter}{Passed to \code{.mor_bjs_solve}. Defaults to \code{2000L}.}
\item{tol}{Passed to \code{.mor_bjs_solve}. Defaults to \code{1e-13}.}
}
\description{
unit-clustered variance.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
