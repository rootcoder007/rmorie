% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpikeInfo}
\alias{SpikeInfo}
\title{I = H(R) - H(R|S), bits.  H_noise is the STIMULUS-WEIGHTED average of}
\usage{
SpikeInfo(spike, stim, nbins = 2L)
}
\arguments{
\item{spike}{See Usage.}
\item{stim}{See Usage.}
\item{nbins}{Defaults to \code{2L}.}
}
\description{
the per-stimulus entropies; weighting them equally inflates I whenever the classes are unbalanced.  Equal-COUNT bins, not equal-width: on a skewed count distribution equal-width bins report I near 0 regardless of the truth.  Strong et al. (1998); biased upward at small n, no correction applied.  NOT in Schabenberger & Gotway.
}
\value{
A list with \code{information}, \code{h_total}, \code{h_noise}, \code{n_stimuli}, \code{nbins}, \code{n_per_cell}, \code{bits}, \code{biased_upward_at_small_n}, \code{equal_count_bins}, \code{n}, \code{method}.
}
\keyword{internal}
