% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_entropy_balance}
\alias{.morie_entropy_balance}
\title{.morie_entropy_balance}
\usage{
.morie_entropy_balance(t_mask, X, max_iter = 200L, tol = 1e-08)
}
\arguments{
\item{t_mask}{A flag; the body branches on it.}
\item{X}{A matrix; indexed by row and column.}
\item{max_iter}{Defaults to \code{200L}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
Entropy balancing (Hainmueller 2012), ATT flavour: reweight controls so their covariate means equal the treated means. Solves the convex dual min_l log(sum_c exp(-Xc l)) + mbar\\' l via BFGS with an analytic gradient; weights are the softmax of -Xc l, rescaled to sum to the number of controls (the ebal::ebalance convention).
}
\value{
A list with \code{w}, \code{converged}, \code{max_imbalance}.
}
\keyword{internal}
