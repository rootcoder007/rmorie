% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ess_ipseq}
\alias{morie_ess_ipseq}
\title{Effective sample size from the initial-positive autocorrelations}
\usage{
morie_ess_ipseq(x)
}
\arguments{
\item{x}{Numeric vector of draws.}
}
\description{
Sums autocorrelations from lag 1 while they stay above 0.05 and returns \code{n / (1 + 2 sum rho)} -- the same initial-positive- sequence estimator that \code{stats::acf} uses by default and that the Python arm\'s \code{baygsl.effective_sample_size} reproduces iteration for iteration. Named with the \code{_ipseq} suffix to avoid clashing with the Kish-weights ESS that ships under the same root in this package.
}
\value{
Estimated ESS.
}
\keyword{internal}
