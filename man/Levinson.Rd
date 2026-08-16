% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Levinson}
\alias{Levinson}
\title{Eqs (7.37)-(7.39).  Init eps_0 = phi(0); for i = 1..P:}
\usage{
Levinson(acf, order = NULL)
}
\arguments{
\item{acf}{See Usage.}
\item{order}{Defaults to \code{NULL}.}
}
\description{
gamma_i = -(1/eps_{i-1})[phi(i) + sum_j a_{i-1,j} phi(i-j)] a_{i,i} = gamma_i; a_{i,j} = a_{i-1,j} + gamma_i a_{i-1,i-j} eps_i = (1 - gamma_i^2) eps_{i-1} The book states the error is monotone and the model is stable exactly when every |gamma_i| < 1; both are checked, not trusted.
}
\value{
A list with \code{a}, \code{reflection}, \code{error}, \code{errors}, \code{gain}, \code{order}, \code{stable}, \code{monotone}, \code{normalized_error}, \code{sign_convention}, \code{method}.
}
\keyword{internal}
