% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tau_numeric}
\alias{.morie_tau_numeric}
\title{Tau = 4 int int C dC - 1 on a grid; used for families whose closed}
\usage{
.morie_tau_numeric(family, theta = NULL, nu = NULL, n = 200L)
}
\arguments{
\item{family}{Passed to \code{morie_copula_cdf}.}
\item{theta}{Passed to \code{morie_copula_cdf}.}
\item{nu}{Passed to \code{morie_copula_cdf}.}
\item{n}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.}
}
\description{
form Czado\\'s Table 3.2 does not give.
}
\value{
A numeric value.
}
\keyword{internal}
