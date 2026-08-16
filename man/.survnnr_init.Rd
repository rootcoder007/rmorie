% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.survnnr_init}
\alias{.survnnr_init}
\title{.survnnr_init}
\usage{
.survnnr_init(d, hidden, seed)
}
\arguments{
\item{d}{See Usage.}
\item{hidden}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Passed to \code{.ghc_rng}.}
}
\description{
A step of the survnnr_native implementation. Called by \code{morie_survnnr_fit}. See the file header for the source the module follows.
}
\value{
A list with \code{W}, \code{b}.
}
\references{
----------
Katzman, J. L., Shaham, U., Cloninger, A., Bates, J., Jiang, T. & Kluger, Y.
(2018) "DeepSurv: personalized treatment recommender system using a Cox
proportional hazards deep neural network", BMC Medical Research Methodology
18, 24, doi:10.1186/s12874-018-0482-1. Sec. "DeepSurv" for the architecture
and for the objective, the average negative log partial likelihood with L2
regularisation.

Cox, D. R. (1972) "Regression Models and Life-Tables", Journal of the Royal
Statistical Society. Series B 34(2), 187-220, doi:10.1111/j.2517-6161.1972
.tb00899.x, for the partial likelihood and the fact that it leaves the
baseline hazard unspecified.

Breslow, N. (1974) "Covariance Analysis of Censored Survival Data",
Biometrics 30(1), 89-99, doi:10.2307/2529620, for the tie handling and the
baseline cumulative hazard estimator used here.
}
\keyword{internal}
\examples{
.survnnr_init(d = 5L, hidden = c(1, 2, 3, 4, 5, 6, 7, 8), seed = 5L)
}
