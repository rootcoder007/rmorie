% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flxipt_nnls_simplex}
\alias{.flxipt_nnls_simplex}
\title{Nnls_simplex: convex combination minimising ||y - Z a||^2 ----}
\usage{
.flxipt_nnls_simplex(Z, y, iters = 8000, tol = 1e-14)
}
\arguments{
\item{Z}{A matrix; passed to \code{nrow}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{8000}.}
\item{tol}{Defaults to \code{1e-14}.}
}
\description{
Accelerated projected gradient with step from the largest eigenvalue of the Gram matrix (estimated by power iteration).
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
