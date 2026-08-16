% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_fit_competing}
\alias{morie_survvae_fit_competing}
\title{One fit per risk, other causes treated as censoring.}
\usage{
morie_survvae_fit_competing(X, times, causes, K = 3L, primitive = "weibull", alpha = 1, prior = 0, seed = 0)
}
\arguments{
\item{causes}{Integer vector of cause labels (0 = censored,   \code{>0} = competing events).}
}
\description{
For each non-zero cause label \code{lab}, refits the model treating \code{lab} as the event of interest and every other non-zero cause as independent censoring (cause 0 is already censored).
}
\value{
Named list with \code{estimate} (number of risks),   \code{risks} (sorted unique non-zero labels), \code{fits}   (one fit per risk keyed by the label as a string) and   \code{method}.
}
\keyword{internal}
