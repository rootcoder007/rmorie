% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlroad_score_spans_eic}
\alias{.tlroad_score_spans_eic}
\title{.tlroad_score_spans_eic}
\usage{
.tlroad_score_spans_eic(A, Y, Q1, Q0, g, h = 1e-06)
}
\arguments{
\item{A}{See Usage.}
\item{Y}{See Usage.}
\item{Q1}{See Usage.}
\item{Q0}{See Usage.}
\item{g}{See Usage.}
\item{h}{Defaults to \code{1e-06}.}
}
\description{
Part of the tlroad_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{score}, \code{eic_component}, \code{difference}, \code{spans}, \code{note}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science: Causal Inference for Complex Longitudinal Studies, Springer,
doi:10.1007/978-3-319-65304-4. Chap. 1 (the roadmap: data as a random
variable; a statistical model representing true knowledge of the
experiment; translation of the scientific question into a statistical
target parameter; TMLE with inference; the three requirements of a
TMLE -- a target parameter defined as a mapping from an infinite
dimensional parameter, a least favorable submodel through the initial
estimator, and a loss function whose score condition on the submodel
spans the efficient score, so the resulting substitution estimator
solves the efficient score equation, giving double robustness and
asymptotic efficiency; the use of a super learner for the initial
fit; and the warning that an untargeted fit is overly biased and not
normally distributed).

van der Laan, M. J. & Rose, S. (2011) Targeted Learning: Causal
Inference for Observational and Experimental Data, Springer,
doi:10.1007/978-1-4419-9782-1. The first book, which this one is a
sequel to.
}
\keyword{internal}
