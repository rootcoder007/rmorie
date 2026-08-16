% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Hyperpl}
\alias{Hyperpl}
\title{Hyperplane definition and side (MVSML eqs. 9.1-9.3)}
\usage{
Hyperpl(X, beta0, beta)
}
\description{
beta_0 + beta_1 x_1 + ... + beta_p x_p = 0 defines a (p-1)-dimensional flat subspace (eq. 9.1 for p = 3, eq. 9.2 in general).  A left-hand side < 0 satisfies (9.3) and puts the point on one side, > 0 satisfies (9.4) and puts it on the other.
}
\value{
A list with \code{value}, \code{side}, \code{below}, \code{above}, \code{on_plane}, \code{distance}, \code{norm_beta}.
}
\keyword{internal}
