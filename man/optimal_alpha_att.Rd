% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{optimal_alpha_att}
\alias{optimal_alpha_att}
\title{Theorem 5.3: the one-sided threshold for the effect on the treated}
\usage{
optimal_alpha_att(pscore, treated, tol = 1e-12, max_iter = 200)
}
\arguments{
\item{pscore}{Propensity scores.}
\item{treated}{Treatment indicators.}
\item{tol}{Convergence tolerance.}
\item{max_iter}{Maximum iterations.}
}
\description{
A*_t = {x : e(x) <= alpha_t}, with alpha_t = 1 (no trimming) when sup_x 1 / (1 - e(x)) <= 2 E[1 / (1 - e(X)) | W = 1], and otherwise solving 1 / (1 - alpha_t) = 2 E[1 / (1 - e(X)) | W = 1, e(X) <= alpha_t]. Homoskedasticity only, as in the paper.
}
\value{
A list with \code{alpha_t}, \code{keep}, \code{trim},   \code{no_trimming}.
}
\keyword{internal}
