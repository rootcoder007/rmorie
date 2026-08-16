% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_estimate_aipw}
\alias{morie_estimate_aipw}
\title{morie_estimate_aipw}
\usage{
morie_estimate_aipw(data, treatment, outcome, covariates, propensity_col = NULL, outcome_model = c("linear", "logistic"), trim = c(0.01, 0.99), trim_type = "value", ps_model = "mle", ridge_lambda = 1, outcome_fit = "separate")
}
\arguments{
\item{data}{A vector; indexed elementwise.}
\item{treatment}{Passed to \code{morie_estimate_propensity_scores}.}
\item{outcome}{See Usage.}
\item{covariates}{Passed to \code{morie_estimate_propensity_scores}.}
\item{propensity_col}{Defaults to \code{NULL}.}
\item{outcome_model}{Passed to \code{.mor_om_fit_predict}. Defaults to \code{c("linear", "logistic")}.}
\item{trim}{Passed to \code{.mor_trim_ps}. Defaults to \code{c(0.01, 0.99)}.}
\item{trim_type}{Passed to \code{.mor_trim_ps}. Defaults to \code{"value"}.}
\item{ps_model}{Passed to \code{morie_estimate_propensity_scores}. Defaults to \code{"mle"}.}
\item{ridge_lambda}{Passed to \code{morie_estimate_propensity_scores}. Defaults to \code{1}.}
\item{outcome_fit}{One of \code{"pooled"}, \code{"separate"}. Defaults to \code{"separate"}.}
}
\description{
A step of the causal implementation. Called by \code{morie_dag_estimate}, \code{morie_estimate_gate}, \code{morie_gate} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{n_used}, \code{n_discarded}, \code{estimand}, \code{ate}, \code{se}, \code{ci_lower}, \code{ci_upper}, \code{n}.
}
\keyword{internal}
\examples{
df <- data.frame(t = rbinom(60, 1, 0.4), x = rnorm(60))
morie_estimate_aipw(df, "t", "y", "x", outcome_model = "linear")
}
