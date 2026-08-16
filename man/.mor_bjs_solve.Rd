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
\item{obs}{A matrix; passed to \code{nrow}.}
\item{u_a}{Numeric; combined arithmetically in the body.}
\item{u_l}{Numeric; combined arithmetically in the body.}
\item{u_b}{Numeric; combined arithmetically in the body.}
\item{Xc}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000L}.}
\item{tol}{Defaults to \code{1e-13}.}
}
\description{
restricted to `obs`, plus optional covariate coefficients.
}
\value{
A list with \code{a}, \code{lam}, \code{b}.
}
\keyword{internal}
