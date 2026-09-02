% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{negbinom_offspring}
\alias{negbinom_offspring}
\title{negbinom_offspring}
\usage{
negbinom_offspring(R0, dispersion, e) 
}
\arguments{
\item{R0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{dispersion}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{e}{Passed to \code{.gamma_draw}.}
}
\description{
A step of the ttrace_native implementation. Called by \code{simulate_outbreak}. See the file header for the source the module follows.
}
\value{
The value of \code{.poisson_draw}.
}
\keyword{internal}
\examples{
e <- rmorie:::.ghc_rng(1)
replicate(5, negbinom_offspring(R0 = 2.5, dispersion = 0.16, e))
}
