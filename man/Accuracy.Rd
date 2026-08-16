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
\item{table}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{tp}{The body requires: give a 2x2 table or all four of tp, tn, fp, fn.}
\item{tn}{The body requires: give a 2x2 table or all four of tp, tn, fp, fn.}
\item{fp}{The body requires: give a 2x2 table or all four of tp, tn, fp, fn.}
\item{fn}{The body requires: give a 2x2 table or all four of tp, tn, fp, fn.}
\item{prevalence}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_asfrac}.}
\item{kind}{Defaults to \code{NULL}.}
\item{exact}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the rangayyan_class implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{accuracy}, \code{kind}, \code{raw_accuracy}, \code{weighted_accuracy}, \code{balanced_accuracy}, \code{sensitivity}, \code{specificity}, \code{prevalence}, \code{test_set_prevalence}, \code{counts}, \code{n}, \code{exact}, \code{prior_weighted}, \code{balanced_is_eq_10_102_at_one_half}, \code{eq_10_103_is_eq_10_102_at_the_test_set_prevalence}, \code{method}.
}
\keyword{internal}
