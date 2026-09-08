% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlsurvy_horvitz_thompson}
\alias{morie_tlsurvy_horvitz_thompson}
\title{Horvitz_thompson: The design-unbiased mean: (1/N) sum_\{i in S\}}
\usage{
morie_tlsurvy_horvitz_thompson(values, pi, selected, N = NULL) 
}
\arguments{
\item{values}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{pi}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{selected}{Coerced to integer by the body, with \code{as.integer}.}
\item{N}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
y_i/pi_i Unbiased for the FULL-data mean, which is the quantity of interest -- the sample is a computational device, not the population.
}
\value{
A list with \code{estimate}, \code{se}, \code{n_used}, \code{N}.
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
morie_tlsurvy_horvitz_thompson(values = c(1, 2, 3, 4, 5, 6, 7, 8),
  pi = c(1, 2, 3, 4, 5, 6, 7, 8), selected = c(1, 2, 3, 4, 5, 6, 7, 8))
}
