% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlsurvy_inclusion_probabilities}
\alias{morie_tlsurvy_inclusion_probabilities}
\title{Inclusion_probabilities: Choose pi_i, summing to n}
\usage{
morie_tlsurvy_inclusion_probabilities(V, n, design = "adaptive", influence = NULL, floor = 0.01)
}
\arguments{
\item{V}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
\item{design}{One of \code{"proportional"}, \code{"uniform"}. Defaults to \code{"adaptive"}.}
\item{influence}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{floor}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.}
}
\description{
adaptive sets pi_i proportional to the expected influence given V_i, which minimises the variance for a fixed n. A floor keeps every unit reachable -- a zero inclusion probability makes the estimand unidentifiable for that stratum.
}
\value{
A list with \code{pi}, \code{design}, \code{n_expected}, \code{N}, \code{min_pi}, \code{note}.
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
\examples{
set.seed(1)
morie_tlsurvy_inclusion_probabilities(V = runif(20), n = 10,
                                      design = "adaptive",
                                      influence = abs(rnorm(20)))
}
