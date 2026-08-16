% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pmpfit_tail}
\alias{morie_pmpfit_tail}
\title{Tail comparison across discounts}
\usage{
morie_pmpfit_tail(n, theta = 1, alphas = c(0, 0.3, 0.6))
}
\arguments{
\item{n}{Number of draws.}
\item{theta}{Concentration, default 1.}
\item{alphas}{Numeric vector of discounts, default c(0, 0.3, 0.6).}
}
\description{
Computes the expected number of clusters at the same theta for a sequence of discounts, so the log-vs-power growth is measured rather than quoted.
}
\value{
A list with `estimate`, `expected_clusters`, `n`, `theta`,   `monotone_in_alpha`, `method`, `note`.
}
\keyword{internal}
