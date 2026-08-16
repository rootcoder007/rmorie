% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Accuracy}
\alias{Accuracy}
\title{Accuracy}
\usage{
Accuracy(table = NULL, tp = NULL, tn = NULL, fp = NULL, fn = NULL, prevalence = NULL, kind = NULL, exact = FALSE)
}
\arguments{
\item{table}{Defaults to \code{NULL}.}
\item{tp}{Defaults to \code{NULL}.}
\item{tn}{Defaults to \code{NULL}.}
\item{fp}{Defaults to \code{NULL}.}
\item{fn}{Defaults to \code{NULL}.}
\item{prevalence}{Defaults to \code{NULL}.}
\item{kind}{Defaults to \code{NULL}.}
\item{exact}{Defaults to \code{FALSE}.}
}
\description{
Part of the rangayyan_class implementation; see the file header for the source it follows.
}
\value{
A list with \code{accuracy}, \code{kind}, \code{raw_accuracy}, \code{weighted_accuracy}, \code{balanced_accuracy}, \code{sensitivity}, \code{specificity}, \code{prevalence}, \code{test_set_prevalence}, \code{counts}, \code{n}, \code{exact}, \code{prior_weighted}, \code{balanced_is_eq_10_102_at_one_half}, \code{eq_10_103_is_eq_10_102_at_the_test_set_prevalence}, \code{method}.
}
\keyword{internal}
