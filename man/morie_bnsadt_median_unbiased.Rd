% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnsadt_median_unbiased}
\alias{morie_bnsadt_median_unbiased}
\title{Median-unbiased correction of a single published estimate}
\usage{
morie_bnsadt_median_unbiased(x, sigma, family = "symmetric_step", params = numeric(0), lo = NULL, 
    hi = NULL) 
}
\arguments{
\item{x}{The published estimate.}
\item{sigma}{Its standard error.}
\item{family}{A name of the selection family.}
\item{params}{The free publication probabilities.}
\item{lo}{Lower end of the bracket.}
\item{hi}{Upper end of the bracket.}
}
\description{
The theta for which the published X has median x, solved by bisection; the published CDF is decreasing in theta, so the root is unique.
}
\value{
The corrected estimate.
}
\keyword{internal}
\examples{
morie_bnsadt_median_unbiased(x = 0.25, sigma = 0.1,
                             family = "symmetric_step", params = 0.3)
}
