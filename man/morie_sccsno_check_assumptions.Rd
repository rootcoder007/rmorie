% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sccsno_check_assumptions}
\alias{morie_sccsno_check_assumptions}
\title{morie_sccsno_check_assumptions}
\usage{
morie_sccsno_check_assumptions(fit_with_pre, pre_index = 0, tol = 0.25)
}
\arguments{
\item{fit_with_pre}{A list; the body reads \code{$relative_incidence} from it.}
\item{pre_index}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.25}.}
}
\description{
A step of the sccsno_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{pre_exposure_ri}, \code{consistent_with_design}, \code{tolerance_log}, \code{interpretation}.
}
\references{
----------
Farrington, C. P. (1995) "Relative Incidence Estimation from Case
Series for Vaccine Safety Evaluation", Biometrics 51(1), 228-235.
JSTOR stable URL https://www.jstor.org/stable/2533328. Secs. 2-3.

Whitaker, H. J., Farrington, C. P., Spiessens, B. & Musonda, P.
(2006) "Tutorial in biostatistics: The self-controlled case series
method", Statistics in Medicine 25, 1768-1797, doi:10.1002/sim.2302.
}
\keyword{internal}
\examples{
set.seed(1)
cases <- lapply(1:12, function(i) {
  expo <- runif(1, 60, 300)
  n_ev <- 1 + rbinom(1, 2, 0.5)
  ev <- sort(c(runif(max(n_ev - 1, 0), 0, 365),
               expo + runif(1, 0, 30)))
  list(start = 0, end = 365, exposure = expo, events = ev)
})
fit <- morie_sccsno_fit(cases,
                        risk_periods = list(c(-30, 0), c(0, 30)))
r <- morie_sccsno_check_assumptions(fit, pre_index = 0)
str(r, max.level = 1)
}
