% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{OtAdapt}
\alias{OtAdapt}
\title{\\' Adapt source samples to a target domain through an OT plan}
\usage{
OtAdapt(Xs, Xt, epsilon, n_iter = 1000L)
}
\arguments{
\item{Xs}{A matrix; indexed by row and column.}
\item{Xt}{A matrix; indexed by row and column.}
\item{epsilon}{Numeric; combined arithmetically in the body.}
\item{n_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1000L}.}
}
\description{
\\' \\' The entropy-regularised plan of equation (9) is found by a fixed \\' number of Sinkhorn-Knopp sweeps, then the source samples are moved by \\' the barycentric mapping of equation (14), \\' Xs_hat = diag(gamma 1_nt)^-1 gamma Xt, with squared Euclidean cost. \\' \\'
}
\value{
A list with \code{Xs_adapted}, \code{gamma}, \code{cost}, \code{transport_cost}, \code{row_error}, \code{col_error}, \code{ns}, \code{nt}, \code{d}, \code{epsilon}, \code{n_iter}, \code{method}.
}
\keyword{internal}
