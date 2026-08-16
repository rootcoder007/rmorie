% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{projected_gradient}
\alias{projected_gradient}
\title{projected_gradient}
\usage{
projected_gradient(f, grad, x0, project, step = NULL, rule = "backtracking", max_iter = 2000, tol = 1e-10)
}
\arguments{
\item{f}{See Usage.}
\item{grad}{See Usage.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{project}{See Usage.}
\item{step}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{rule}{One of \code{"fista"}, \code{"fixed"}. Defaults to \code{"backtracking"}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{2000}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
A step of the pgdsdg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{x}, \code{fun}, \code{iterations}, \code{history}, \code{step}, \code{rule}, \code{n_backtracks}, \code{fixed_point_residual}, \code{converged}, \code{monotone}, \code{method}.
}
\references{
Goldstein, A. A. (1964) "Convex programming in Hilbert space",
  Bulletin of the American Mathematical Society 70(5), 709-710,
  doi:10.1090/S0002-9904-1964-11178-2.
Levitin, E. S. & Polyak, B. T. (1966) "Constrained minimization
  methods", USSR Computational Mathematics and Mathematical Physics
  6(5), 1-50, doi:10.1016/0041-5553(66)90114-5.
Beck, A. & Teboulle, M. (2009) "A fast iterative shrinkage-
  thresholding algorithm for linear inverse problems", SIAM Journal
  on Imaging Sciences 2(1), 183-202, doi:10.1137/080716542.
}
\keyword{internal}
