% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmldyn}
\alias{morie_tmldyn}
\alias{morie_tmledynamicregime}
\title{Mean outcome under the (V-)optimal dynamic treatment rule}
\usage{
morie_tmldyn(y, treatment_history, covariate_history, regime = "optimal", 
    method = "cv-tmle", n_folds = 10, V0 = NULL, V1 = NULL, trim = 0.01, 
    known_g = NULL, ridge = 1e-08, level = 0.95) 

morie_tmledynamicregime(y, treatment_history, covariate_history, regime = "optimal", 
    method = "cv-tmle", n_folds = 10, V0 = NULL, V1 = NULL, trim = 0.01, 
    known_g = NULL, ridge = 1e-08, level = 0.95) 
}
\arguments{
\item{y}{Outcome vector.}
\item{treatment_history}{n-by-2 binary matrix.}
\item{covariate_history}{List \code{[L0, L1]}.}
\item{regime}{\code{"optimal"} or supplied rule.}
\item{method}{Method choice.}
\item{n_folds}{Number of folds.}
\item{V0}{First-stage summary.}
\item{V1}{Second-stage summary.}
\item{trim}{Propensity truncation.}
\item{known_g}{Optional known treatment probabilities.}
\item{ridge}{Ridge regulariser.}
\item{level}{Confidence level.}
}
\description{
\code{method} is one of \code{"cv-tmle"} (the default, Sec. 22.6), \code{"tmle"} (same fluctuation without the split), \code{"ipw"} (Horvitz-Thompson mean under the rule), \code{"gcomp"} (the untargeted sequential regression).
}
\value{
A list with \code{estimate}, \code{se}, \code{n}, \code{ci},   \code{level}, \code{d0}, \code{d1}, \code{blip1}, \code{blip2},   \code{treated_first}, \code{treated_second}, \code{eic_mean},   \code{epsilon}, \code{max_weight}, \code{min_g0}, \code{min_g1},   \code{known_g}, \code{exceptional_share_1},   \code{exceptional_share_2}, \code{value_gcomp},   \code{best_static}, \code{n_folds}, \code{method},   \code{rule_source}, \code{algorithm}, plus four \code{static_*}   keys.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 80
L0 <- rnorm(n)
A0 <- rbinom(n, 1, 0.5)
L1 <- rnorm(n) + 0.4 * A0
A1 <- rbinom(n, 1, 0.5)
y <- plogis(0.3 * A0 + 0.5 * A1 + 0.4 * L0 + rnorm(n, 0, 0.4))
r <- morie_tmldyn(y, cbind(A0, A1), list(L0, L1), method = "tmle")
str(r, max.level = 1)
}
