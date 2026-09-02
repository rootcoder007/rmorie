% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_estimate_propensity_scores}
\alias{morie_estimate_propensity_scores}
\title{morie_estimate_propensity_scores}
\usage{
morie_estimate_propensity_scores(data, treatment, covariates, trim = c(0.01, 0.99), trim_type = "value", 
    ps_model = "mle", ridge_lambda = 1) 
}
\arguments{
\item{data}{Passed to \code{.fit_propensity}.}
\item{treatment}{Passed to \code{.fit_propensity}.}
\item{covariates}{Passed to \code{.fit_propensity}.}
\item{trim}{Passed to \code{.mor_trim_ps}. Defaults to \code{c(0.01, 0.99)}.}
\item{trim_type}{Passed to \code{.mor_trim_ps}. Defaults to \code{"value"}.}
\item{ps_model}{Passed to \code{.fit_propensity}. Defaults to \code{"mle"}.}
\item{ridge_lambda}{Passed to \code{.fit_propensity}. Defaults to \code{1}.}
}
\description{
A step of the causal implementation. Called by \code{morie_estimate_aipw}, \code{morie_estimate_atc}, \code{morie_estimate_ate} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{.mor_trim_ps}.
}
\keyword{internal}
\examples{
df <- data.frame(t = rbinom(60, 1, 0.4), x = rnorm(60))
morie_estimate_propensity_scores(df, "t", "x")
}
