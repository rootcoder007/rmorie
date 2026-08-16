% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sample_neg_bin}
\alias{.sample_neg_bin}
\title{Gamma-Poisson mixture sampler}
\usage{
.sample_neg_bin(mu, alpha, e)
}
\arguments{
\item{mu}{Numeric mean.}
\item{alpha}{Numeric overdispersion.}
\item{e}{Generator environment from \code{.ghc_rng}.}
}
\description{
The standard construction: \code{NB(mu, alpha)} as a Poisson with rate drawn from \code{Gamma(1/alpha, mu * alpha)}. Uses the shared generator so the same seed reproduces the same draws.
}
\value{
Numeric draw.
}
\keyword{internal}
