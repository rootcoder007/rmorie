% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{private_ci}
\alias{private_ci}
\title{An interval that accounts for the mechanism\'s own variance}
\usage{
private_ci(value, sensitivity, epsilon, se, seed = 0, level = 1.96)
}
\arguments{
\item{value}{Non-private statistic.}
\item{sensitivity}{L1 sensitivity.}
\item{epsilon}{Privacy budget.}
\item{se}{Sampling standard error.}
\item{seed}{Seed for the shared generator.}
\item{level}{Multiplier (e.g. 1.96 for 95 percent).}
}
\description{
Var = Var_sampling + 2(Delta f / epsilon)^2.
}
\value{
A list with \code{estimate}, \code{se_private},   \code{se_sampling}, \code{ci}, \code{width_ratio},   \code{epsilon}.
}
\keyword{internal}
\examples{
r <- private_ci(value = 0.4, sensitivity = 0.02, epsilon = 1, se = 0.05)
str(r, max.level = 1)
}
