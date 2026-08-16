% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pmpfit_expected}
\alias{morie_pmpfit_expected}
\title{Expected number of clusters under Pitman-Yor}
\usage{
morie_pmpfit_expected(n, alpha, theta)
}
\arguments{
\item{n}{Number of draws, at least 1.}
\item{alpha}{Discount.}
\item{theta}{Concentration.}
}
\description{
Computes E[K_n] by the exact recursion ek_{i+1} = ek_i + (theta + ek_i * alpha) / (theta + i). The DP\'s count grows like theta*log(n); a positive discount makes it grow like n^alpha.
}
\value{
A list with `expected`, `n`, `alpha`, `theta`, `regime`,   `note`.
}
\keyword{internal}
