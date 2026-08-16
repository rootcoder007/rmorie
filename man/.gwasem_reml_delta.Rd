% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwasem_reml_delta}
\alias{.gwasem_reml_delta}
\title{.gwasem_reml_delta}
\usage{
.gwasem_reml_delta(y, X, evals, evecs, ml = FALSE, lo = -10, hi = 10, n_grid = 100L, refine = 60L)
}
\arguments{
\item{y}{A matrix; passed to \code{\%*\%}.}
\item{X}{A matrix; passed to \code{ncol}.}
\item{evals}{Numeric; combined arithmetically in the body.}
\item{evecs}{A matrix; passed to \code{t}.}
\item{ml}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{lo}{Passed to \code{seq}. Defaults to \code{-10}.}
\item{hi}{Passed to \code{seq}. Defaults to \code{10}.}
\item{n_grid}{Numeric; combined arithmetically in the body. Defaults to \code{100L}.}
\item{refine}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.}
}
\description{
A step of the gwasem_native implementation. Called by \code{morie_gwasem_reml}. See the file header for the source the module follows.
}
\value{
A list with \code{delta}, \code{sigma_a2}, \code{sigma_e2}, \code{loglik}.
}
\keyword{internal}
