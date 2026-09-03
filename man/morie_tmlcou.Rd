% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcou}
\alias{morie_tmlcou}
\alias{morie_tmlcountoutcome}
\title{TMLE of the mean-outcome contrast for a count or bounded outcome}
\usage{
morie_tmlcou(y, D, X, offset = NULL, g = NULL, Q1 = NULL, Q0 = NULL, lower = NULL, 
    upper = NULL, iters = 100) 

morie_tmlcountoutcome(y, D, X, offset = NULL, g = NULL, Q1 = NULL, Q0 = NULL, lower = NULL, 
    upper = NULL, iters = 100) 
}
\arguments{
\item{y}{Outcome vector.}
\item{D}{Treatment indicator vector.}
\item{X}{Covariate matrix (rows are observations).}
\item{offset}{Optional exposure time.}
\item{g}{Optional propensity score.}
\item{Q1}{Optional potential-outcome regression under treatment.}
\item{Q0}{Optional potential-outcome regression under control.}
\item{lower}{Lower bound for the outcome.}
\item{upper}{Upper bound for the outcome.}
\item{iters}{Maximum number of Newton steps.}
}
\description{
\code{offset} supplies exposure time, in which case the estimand is a rate. Nuisance fits may be supplied; otherwise they are fitted by logistic and least-squares regression on \code{X}.
}
\value{
A \code{RichResult}-style list with \code{estimate},   \code{psi}, \code{epsilon}, \code{se}, \code{ci},   \code{mean_eic}, \code{solves_eic}, \code{scale},   \code{mean_treated}, \code{mean_control}, \code{in_range},   \code{rate_scale}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
g <- c(0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L)
x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
res <- morie_tmlcou(y = y, D = g, X = x)
res
}
