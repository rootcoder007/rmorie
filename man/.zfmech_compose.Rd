% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.zfmech_compose}
\alias{.zfmech_compose}
\title{Lemma 1.7: rho simply adds}
\usage{
.zfmech_compose(rhos)
}
\arguments{
\item{rhos}{Numeric vector of rho values, each non-negative.}
}
\description{
Lemma 1.7: rho simply adds
}
\value{
A list with \code{rho} (the sum), \code{k} (the count),   and a \code{note} explaining that composition is additive in   rho with no delta budget and no advanced composition theorem.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.zfmech_compose(V)
}
