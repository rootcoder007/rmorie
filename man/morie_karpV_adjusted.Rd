% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_karpV_adjusted}
\alias{morie_karpV_adjusted}
\title{Adjusted fitness}
\usage{
morie_karpV_adjusted(raw)
}
\arguments{
\item{raw}{raw fitness, smaller being better.}
}
\description{
Adjusted fitness
}
\value{
1/(1+raw), which is what selection uses.
}
\keyword{internal}
