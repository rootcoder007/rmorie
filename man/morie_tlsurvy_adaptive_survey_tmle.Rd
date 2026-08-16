% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlsurvy_adaptive_survey_tmle}
\alias{morie_tlsurvy_adaptive_survey_tmle}
\title{Adaptive_survey_tmle: Sample by the adaptive design, then run the}
\usage{
morie_tlsurvy_adaptive_survey_tmle(V, influence_proxy, full_estimator, n, seed = 0)
}
\arguments{
\item{V}{Passed to \code{morie_tlsurvy_inclusion_probabilities}.}
\item{influence_proxy}{Passed to \code{morie_tlsurvy_inclusion_probabilities}.}
\item{full_estimator}{Accepted by the signature and not used anywhere in the body.}
\item{n}{Passed to \code{morie_tlsurvy_inclusion_probabilities}.}
\item{seed}{Passed to \code{morie_tlsurvy_draw_sample}. Defaults to \code{0}.}
}
\description{
estimator on the sample. Reports both error sources: the sampling variance from using n of N, and the estimator\'s own standard error.
}
\value{
A list with \code{estimate}, \code{psi}, \code{se_estimator}, \code{n_used}, \code{N}, \code{sampling_fraction}, \code{inclusion_probabilities}, \code{method}, \code{note}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 29 (Chambaz,
Joly & Mary): building a confidence interval for a real-valued
pathwise differentiable parameter from N independent observations when
N is so large that not all data can be used; the two-part response of
selecting n among N randomly with UNEQUAL INCLUSION PROBABILITIES and
adapting TMLE to the resulting smaller data set; the asymptotics with
N to infinity and n to infinity such that n/N goes to zero; the
selection as the random outcome of a survey sampling design; and the
assumption that each observation is summarised by a low-dimensional
V_i with V_1, ..., V_N all observed.

Horvitz, D. G. & Thompson, D. J. (1952) "A Generalization of Sampling
Without Replacement From a Finite Universe", Journal of the American
Statistical Association 47(260), 663-685,
doi:10.1080/01621459.1952.10483446.

Chambaz, A., Joly, E. & Mary, X. (2018) "Targeted Learning Using
Adaptive Survey Sampling", in Targeted Learning in Data Science,
Springer, doi:10.1007/978-3-319-65304-4_29.
}
\keyword{internal}
