% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_bjs_solve}
\alias{.mor_bjs_solve}
\title{Alternating solve of the two-way (unit, period) normal equations}
\usage{
.mor_bjs_solve(obs, u_a, u_l, u_b, Xc, max_iter = 2000L, tol = 1e-13)
}
\arguments{
\item{obs}{See Usage.}
\item{u_a}{See Usage.}
\item{u_l}{See Usage.}
\item{u_b}{See Usage.}
\item{Xc}{See Usage.}
\item{max_iter}{Defaults to \code{2000L}.}
\item{tol}{Defaults to \code{1e-13}.}
}
\description{
restricted to `obs`, plus optional covariate coefficients.
}
\value{
A list with \code{a}, \code{lam}, \code{b}.
}
\keyword{internal}
