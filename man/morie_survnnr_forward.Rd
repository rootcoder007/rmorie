% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survnnr_forward}
\alias{morie_survnnr_forward}
\title{morie_survnnr_forward}
\usage{
morie_survnnr_forward(W, b, x, activation = "tanh") 
}
\arguments{
\item{W}{A vector; its length is taken and its elements indexed.}
\item{b}{A vector; indexed elementwise.}
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{activation}{Passed to \code{.survnnr_act}. Defaults to \code{"tanh"}.}
}
\description{
A step of the survnnr_native implementation. Called by \code{morie_survnnr_fit}, \code{morie_survnnr_risk_score}. See the file header for the source the module follows.
}
\value{
A list with \code{output}, \code{pre}, \code{acts}.
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
morie_survnnr_forward(W = 5L, b = 5L, x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
