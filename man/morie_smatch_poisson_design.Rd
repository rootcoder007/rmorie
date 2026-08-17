% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_smatch_poisson_design}
\alias{morie_smatch_poisson_design}
\title{morie_smatch_poisson_design}
\usage{
morie_smatch_poisson_design(cases, risk_periods, age_breaks = numeric(0))
}
\arguments{
\item{cases}{See Usage.}
\item{risk_periods}{Iterated over elementwise, with \code{lapply}.}
\item{age_breaks}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{numeric(0)}.}
}
\description{
A step of the smatch_native implementation. Called by \code{morie_smatch_sccs_poisson_fit}. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{offset}, \code{X}, \code{n_risk}, \code{n_age}, \code{n_people}, \code{n_rows}.
}
\keyword{internal}
\examples{
cases <- list(
  list(start = 0, end = 365, exposure = 100, events = c(110, 250)),
  list(start = 0, end = 365, exposure = 200, events = c(215)),
  list(start = 0, end = 365, exposure = 50, events = c(60, 70)))
r <- morie_smatch_poisson_design(cases, risk_periods = list(c(0, 30)))
str(r, max.level = 1)
}
