% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wolfedual}
\alias{Wolfedual}
\title{Wolfe dual of a constrained program (MVSML eqs. 9.9-9.14)}
\usage{
Wolfedual(f, grad_f, h = NULL, grad_h = NULL, g = NULL, grad_g = NULL, lam = NULL, alpha = NULL)
}
\description{
(9.12) maximizes L = f - sum_i lambda_i h_i - sum_i alpha_i g_i subject to the stationarity condition (9.13), grad f - sum lambda_i grad h_i - sum alpha_i grad g_i = 0, and to alpha_i >= 0 (9.14).  The book warns under (9.14) that the sign of the inequality term is crucial; its own worked examples supply the constraint in the >= form and subtract it, the convention used here.
}
\value{
A list with \code{L}, \code{stationarity}, \code{max_stationarity}, \code{alpha_nonnegative}, \code{n_equality}, \code{n_inequality}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Wolfedual(V, V)
}
