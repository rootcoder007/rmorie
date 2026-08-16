% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlsieve}
\alias{morie_tlsieve}
\title{morie_tlsieve}
\usage{
morie_tlsieve(F_vac_matched, F_pla_matched, F_vac_mismatched, F_pla_mismatched)
}
\arguments{
\item{F_vac_matched}{See Usage.}
\item{F_pla_matched}{See Usage.}
\item{F_vac_mismatched}{See Usage.}
\item{F_pla_mismatched}{See Usage.}
}
\description{
Part of the tlsieve_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{sieve_effect}, \code{ve_matched}, \code{ve_mismatched}, \code{method}, \code{note}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 11
(Benkeser, Carone & Gilbert): sieve analysis as the study of how
vaccine efficacy varies with viral genetics; the sieve effect at a
locus as the difference in efficacy between matched and mismatched
viruses, and its use in selecting antigens for multivalent vaccines;
the competing risks framework with each genotype a separate endpoint;
cumulative incidence as the cumulative parameter, of greater public
health relevance when vaccine effects wane; the Aalen-Johansen
estimator\'s consistency under uninformative censoring and
nonparametric efficiency absent covariates; the concern of informative
censoring and the routine availability of prognostic covariates; and
the drawback that semiparametric hazard-based alternatives require a
correctly specified finite-dimensional regression model.

Aalen, O. O. & Johansen, S. (1978) "An Empirical Transition Matrix for
Non-Homogeneous Markov Chains Based on Censored Observations",
*Scandinavian Journal of Statistics* 5(3), 141-150. The estimator
being improved on.

Gilbert, P. B., Self, S. G. & Ashby, M. A. (1998) "Statistical methods
for assessing differential vaccine protection against human
immunodeficiency virus types", *Biometrics* 54(3), 799-814,
doi:10.2307/2533838. Sieve analysis.
"""
}
\keyword{internal}
\examples{
morie_tlsieve(F_vac_matched = c(1, 2, 3, 4, 5, 6, 7, 8), F_pla_matched = c(1, 2, 3, 4, 5, 6, 7, 8), F_vac_mismatched = c(1, 2, 3, 4, 5, 6, 7, 8), F_pla_mismatched = c(1, 2, 3, 4, 5, 6, 7, 8))
}
