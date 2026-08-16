% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_tsdae_objective}
\alias{morie_alammar_tsdae_objective}
\title{morie_alammar_tsdae_objective}
\usage{
morie_alammar_tsdae_objective(tokens, delete_ratio = 0.6, seed = 1, reconstruction_logprob = NULL)
}
\arguments{
\item{tokens}{Token vector.}
\item{delete_ratio}{Deletion rate.}
\item{seed}{LCG seed.}
\item{reconstruction_logprob}{Optional   per-ORIGINAL-token log-probs.}
}
\description{
morie_alammar_tsdae_objective
}
\value{
A list with \code{corrupted}, \code{deleted}, \code{actual_delete_ratio}, \code{loss}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
