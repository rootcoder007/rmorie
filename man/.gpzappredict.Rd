% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpzappredict}
\alias{.gpzappredict}
\title{Book erratum: eq. (15.3) as printed drops the mu from the numerator.}
\usage{
.gpzappredict(theta_hat, mu_hat)
}
\arguments{
\item{theta_hat}{See Usage.}
\item{mu_hat}{See Usage.}
}
\description{
The ZAP pmf printed above it, the Var(Y) line below it, and the p.652 estimating equation for mu all carry the mu, so the internally consistent mean (1 - theta) mu / (1 - exp(-mu)) is what is implemented.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
