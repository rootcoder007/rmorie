% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.snpest_ess}
\alias{.snpest_ess}
\title{Effective sample size, 1 / sum w^2 for normalised weights}
\usage{
.snpest_ess(w)
}
\arguments{
\item{w}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the snpest_native implementation. Called by \code{morie_snpest}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
