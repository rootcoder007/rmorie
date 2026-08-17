% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_smatch_sccs_poisson_fit}
\alias{morie_smatch_sccs_poisson_fit}
\title{morie_smatch_sccs_poisson_fit}
\usage{
morie_smatch_sccs_poisson_fit(cases, risk_periods, age_breaks = numeric(0), iters = 200, tol = 1e-12, ridge = 1e-09)
}
\arguments{
\item{cases}{Passed to \code{morie_smatch_poisson_design}.}
\item{risk_periods}{Passed to \code{morie_smatch_poisson_design}.}
\item{age_breaks}{Passed to \code{morie_smatch_poisson_design}. Defaults to \code{numeric(0)}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-12}.}
\item{ridge}{A matrix; passed to \code{diag}. Defaults to \code{1e-09}.}
}
\description{
A step of the smatch_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{relative_incidence}, \code{log_ri}, \code{age_effects}, \code{individual_effects}, \code{coef}, \code{converged}, \code{iterations}, \code{n_rows}, \code{n_people}, \code{method}, \code{identical_to}.
}
\keyword{internal}
\examples{
cases <- list(
  list(start = 0, end = 365, exposure = 100, events = c(110, 250)),
  list(start = 0, end = 365, exposure = 200, events = c(215)),
  list(start = 0, end = 365, exposure = 50, events = c(60, 70)))
r <- morie_smatch_sccs_poisson_fit(cases, risk_periods = list(c(0, 30)))
str(r, max.level = 1)
}
