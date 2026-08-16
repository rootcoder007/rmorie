% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlsurvy_draw_sample}
\alias{morie_tlsurvy_draw_sample}
\title{Draw_sample: Poisson sampling: include unit i with probability pi_i}
\usage{
morie_tlsurvy_draw_sample(pi, seed = 0)
}
\arguments{
\item{pi}{See Usage.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the tlsurvy_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{selected}, \code{n}, \code{fraction}.
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
