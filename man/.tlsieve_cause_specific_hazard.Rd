% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlsieve_cause_specific_hazard}
\alias{.tlsieve_cause_specific_hazard}
\title{.tlsieve_cause_specific_hazard}
\usage{
.tlsieve_cause_specific_hazard(time, event_type, times, weights = NULL)
}
\arguments{
\item{time}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{event_type}{Coerced to integer by the body, with \code{as.integer}.}
\item{times}{A vector; its length is taken and its elements indexed.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlsieve_native implementation. Called by \code{.tlsieve_aalen_johansen}. See the file header for the source the module follows.
}
\value{
A list with \code{hazards}, \code{types}, \code{times}.
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
