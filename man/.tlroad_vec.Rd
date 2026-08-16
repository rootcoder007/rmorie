% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlroad_vec}
\alias{.tlroad_vec}
\title{.tlroad_vec}
\usage{
.tlroad_vec(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlroad_native implementation. Called by \code{.tlroad_eic_ate}, \code{.tlroad_plugin}, \code{.tlroad_score_spans_eic}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
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
