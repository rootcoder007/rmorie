% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmldyn_logit_irls}
\alias{.tmldyn_logit_irls}
\title{Internal: ridge-penalised logistic fit for the treatment mechanism}
\usage{
.tmldyn_logit_irls(X, y, penalty = 0, max_iter = 60L)
}
\arguments{
\item{X}{Design matrix, intercept included.}
\item{y}{Binary treatment.}
\item{penalty}{Ridge penalty; zero gives the unpenalised fit.}
\item{max_iter}{Iteration cap.}
}
\description{
Newton steps on the penalised binomial log-likelihood. The penalty is what keeps the fit finite when the covariates nearly separate the treatment, which is the case a propensity model most often meets and least often survives.
}
\value{
The coefficient vector.
}
\keyword{internal}
