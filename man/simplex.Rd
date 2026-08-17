% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{simplex}
\alias{simplex}
\title{Two-phase primal simplex on Ax = b, x >= 0. Mirrors clpopt.simplex}
\usage{
simplex(c, A, b, rule = "bland", max_iter = 10000, initial_basis = NULL)
}
\arguments{
\item{c}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{A}{Passed to \code{.clpopt_mat}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rule}{Passed to \code{.clpopt_run}. Defaults to \code{"bland"}.}
\item{max_iter}{Passed to \code{.clpopt_run}. Defaults to \code{10000}.}
\item{initial_basis}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the clpopt_native implementation. Called by \code{morie_clpopt}. See the file header for the source the module follows.
}
\value{
The value of \code{.clpopt_report}.
}
\keyword{internal}
\examples{
sf <- standard_form(c = c(-3, -5), A_ub = rbind(c(1, 0), c(0, 2),
                                                c(3, 2)),
                    b_ub = c(4, 12, 18))
r <- simplex(sf$c, sf$A, sf$b)
str(r, max.level = 1)
}
