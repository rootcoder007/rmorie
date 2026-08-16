% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{easy_only_fit}
\alias{easy_only_fit}
\title{easy_only_fit}
\usage{
easy_only_fit(X, y, difficulty, X_test, y_test, quantile = 0.5, updates = 200, seed = 0, n_repeats = 50)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{difficulty}{See Usage.}
\item{X_test}{See Usage.}
\item{y_test}{See Usage.}
\item{quantile}{Defaults to \code{0.5}.}
\item{updates}{Defaults to \code{200}.}
\item{seed}{Defaults to \code{0}.}
\item{n_repeats}{Defaults to \code{50}.}
}
\description{
Part of the prgrl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{easy_only_error}, \code{all_examples_error}, \code{improvement}, \code{n_kept}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
