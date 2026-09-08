% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survnnr_baseline_hazard}
\alias{morie_survnnr_baseline_hazard}
\title{morie_survnnr_baseline_hazard}
\usage{
morie_survnnr_baseline_hazard(fit_result) 
}
\arguments{
\item{fit_result}{A list; the body reads \code{$events}, \code{$risk}, \code{$times} from it.}
}
\description{
A step of the survnnr_native implementation. Called by \code{morie_survnnr_survival_function}. See the file header for the source the module follows.
}
\value{
A list with \code{time}, \code{cumulative_hazard}.
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
