% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.survnnr_grad_wrt_risk}
\alias{.survnnr_grad_wrt_risk}
\title{.survnnr_grad_wrt_risk}
\usage{
.survnnr_grad_wrt_risk(times, events, risk)
}
\arguments{
\item{times}{A vector; its length is taken and its elements indexed.}
\item{events}{A vector; indexed elementwise.}
\item{risk}{A vector; indexed elementwise.}
}
\description{
A step of the survnnr_native implementation. Called by \code{morie_survnnr_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{g}, as built in the body.
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
.survnnr_grad_wrt_risk(times = c(1, 2, 3, 4, 5, 6, 7, 8), events = c(1, 2, 3, 4, 5, 6, 7, 8), risk = c(1, 2, 3, 4, 5, 6, 7, 8))
}
