% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_attrInf}
\alias{morie_attrInf}
\title{Attribute inference by model inversion}
\usage{
morie_attrInf(tree, targets, priors, confusion = NULL, labels = NULL, sensitive = 0L, mode = "blackbox", candidates = NULL, unknown = NULL)
}
\arguments{
\item{tree}{Nested tree: each non-leaf is a list with   \code{feature} (integer index) and \code{branches} (a named   list whose names are the branch values mapping to child   subtrees); each leaf is a list with \code{label} and, for the   white-box route, \code{count}.}
\item{targets}{List of target lists, each with \code{known} (a   named list mapping feature-index-as-string to value),   \code{label}, and, when the truth is known, \code{truth}.}
\item{priors}{Named list mapping feature-index-as-string to a   named numeric vector of marginal probabilities.}
\item{confusion}{Square confusion matrix (counts); required when   \code{mode = "blackbox"}.}
\item{labels}{Optional labels for the confusion-matrix rows;   defaults to \code{0:(nrow-1)}.}
\item{sensitive}{Integer index of the sensitive feature.}
\item{mode}{Either \code{"blackbox"} or \code{"whitebox"}.}
\item{candidates}{Optional list of candidate values for the   sensitive feature; defaults to those appearing on the tree,   falling back to the keys of the sensitive feature\'s prior.}
\item{unknown}{Optional integer vector of further feature indices   whose values are unknown; Equation 1 is summed over them.}
}
\description{
Runs the Fredrikson-Jha-Ristenpart (2015) model inversion attack over a set of targets and scores it. For each target it scores every candidate value of the sensitive feature, multiplies the error-model probability by the marginal priors, and returns the maximum a posteriori estimate. The black-box route uses \code{err(y, y\')} from a confusion matrix; the white-box route additionally weights each root-to-leaf path by its training count, which carries joint-distribution information the marginals cannot reach on their own.
}
\value{
A named list mirroring the Python payload:   \code{estimate}, \code{guesses}, \code{accuracy},   \code{precision}, \code{recall}, \code{false_positives},   \code{true_positives}, \code{mode}, \code{candidates},   \code{n_paths}, \code{n_targets}, \code{method}, \code{note}.
}
\keyword{internal}
