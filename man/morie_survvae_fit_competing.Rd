% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_fit_competing}
\alias{morie_survvae_fit_competing}
\title{One fit per risk, other causes treated as censoring}
\usage{
morie_survvae_fit_competing(X, times, causes, K = 3L, primitive = "weibull", alpha = 1, prior = 0, 
    seed = 0) 
}
\arguments{
\item{causes}{Integer vector of cause labels (0 = censored,   \code{>0} = competing events).}

\item{X}{Argument \code{X}; see Usage.}

\item{times}{Argument \code{times}; see Usage.}

\item{K}{Argument \code{K}; see Usage.}

\item{primitive}{Argument \code{primitive}; see Usage.}

\item{alpha}{Argument \code{alpha}; see Usage.}

\item{prior}{Argument \code{prior}; see Usage.}

\item{seed}{Argument \code{seed}; see Usage.}
}
\description{
For each non-zero cause label \code{lab}, refits the model treating \code{lab} as the event of interest and every other non-zero cause as independent censoring (cause 0 is already censored).
}
\value{
Named list with \code{estimate} (number of risks),   \code{risks} (sorted unique non-zero labels), \code{fits}   (one fit per risk keyed by the label as a string) and   \code{method}.
}
\keyword{internal}
\examples{
morie_survvae_fit_competing(X = c(1, 2, 3, 4, 5, 6, 7, 8), times = c(1, 2, 3, 4, 5, 6, 7, 8), causes = c(1, 2, 3, 4, 5, 6, 7, 8))
}
