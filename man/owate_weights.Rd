% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{owate_weights}
\alias{owate_weights}
\title{Theorem 5.4 / Corollary 5.2: optimal weights}
\usage{
owate_weights(pscore, sigma2_treated = NULL, sigma2_control = NULL) 
}
\arguments{
\item{pscore}{Propensity scores.}
\item{sigma2_treated}{Optional conditional variance under treatment.}
\item{sigma2_control}{Optional conditional variance under control.}
}
\description{
omega*(x) = (sigma1^2 / e + sigma0^2 / (1 - e))^\{-1\}, which is e(x)(1 - e(x)) under homoskedasticity.
}
\value{
Weight vector.
}
\keyword{internal}
\examples{
set.seed(1)
e <- runif(20, 0.05, 0.95)
owate_weights(e)
}
