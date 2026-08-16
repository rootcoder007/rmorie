% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlroad_roadmap}
\alias{.tlroad_roadmap}
\title{.tlroad_roadmap}
\usage{
.tlroad_roadmap(data_description, model_assumptions, target_name, estimator = "TMLE")
}
\arguments{
\item{data_description}{See Usage.}
\item{model_assumptions}{See Usage.}
\item{target_name}{See Usage.}
\item{estimator}{Defaults to \code{"TMLE"}.}
}
\description{
Part of the tlroad_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{steps}, \code{data}, \code{model}, \code{target}, \code{estimator}, \code{note}.
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
