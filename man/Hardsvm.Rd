% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Hardsvm}
\alias{Hardsvm}
\title{Maximum margin classifier (MVSML eqs. 9.6-9.8)}
\usage{
Hardsvm(X, y, ...) 
}
\description{
(9.6) maximizes M subject to sum_j beta_j^2 = 1 and y_i(beta_0 + x_i beta) >= M.  Since M = 1 / ||beta|| once the scale is fixed, that is equivalent to minimizing (1/2)||beta||^2 (9.7) subject to y_i(beta_0 + x_i beta) >= 1 (9.8); the street is 2 / ||beta||.
}
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{beta}, \code{beta0}, \code{norm_beta}, \code{margin}, \code{street_width}, \code{objective}, \code{functional_margin}, \code{min_functional_margin}, \code{constraint_ok}, \code{alpha}, \code{support_vectors}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Hardsvm(V, V)
}
