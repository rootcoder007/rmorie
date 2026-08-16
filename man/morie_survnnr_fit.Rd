% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survnnr_fit}
\alias{morie_survnnr_fit}
\title{morie_survnnr_fit}
\usage{
morie_survnnr_fit(X, times, events, hidden = c(), activation = "tanh", l2 = 0, lr = 0.1, n_epochs = 400, seed = 0, tol = 1e-10)
}
\arguments{
\item{X}{See Usage.}
\item{times}{See Usage.}
\item{events}{See Usage.}
\item{hidden}{Defaults to \code{c()}.}
\item{activation}{Defaults to \code{"tanh"}.}
\item{l2}{Defaults to \code{0}.}
\item{lr}{Defaults to \code{0.1}.}
\item{n_epochs}{Defaults to \code{400}.}
\item{seed}{Defaults to \code{0}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
Part of the survnnr_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{W}, \code{b}, \code{activation}, \code{hidden}, \code{l2}, \code{loss_history}, \code{risk}, \code{centred_risk}, \code{coefficients}, \code{times}, \code{events}, \code{epochs}, \code{ties}, \code{scale_note}, \code{method}.
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
