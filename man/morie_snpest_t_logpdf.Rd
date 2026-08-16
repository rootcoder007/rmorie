% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_snpest_t_logpdf}
\alias{morie_snpest_t_logpdf}
\title{log density of a Student t}
\usage{
morie_snpest_t_logpdf(x, df, loc, scale2)
}
\arguments{
\item{x}{The point.}
\item{df}{Degrees of freedom.}
\item{loc}{Location.}
\item{scale2}{Squared scale.}
}
\description{
Written out rather than taken from dt(): the two languages carry separate implementations and they do not agree in the last digits.
}
\value{
The log density.
}
\references{
  MacEachern, S.N., Clyde, M. and Liu, J.S. (1999) "Sequential
    importance sampling for nonparametric Bayes models: the next
    generation." Canadian Journal of Statistics 27(2), 251-267.
  Fearnhead, P. (2004) "Particle filters for mixture models with an
    unknown number of components." Statistics and Computing 14(1),
    11-21.
  Caron, F., Doucet, A. and Gottardo, R. (2012) "On-line changepoint
    detection and parameter estimation with application to genomic
    data." Statistics and Computing 22(2), 579-595. The ledger cites
    this as 2017; the paper is 2012, and the year is corrected here
    rather than repeated.
  Escobar, M.D. and West, M. (1995) JASA 90(430), 577-588.
}
\keyword{internal}
