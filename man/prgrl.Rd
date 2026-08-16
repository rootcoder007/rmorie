% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{prgrl}
\alias{prgrl}
\title{prgrl}
\usage{
prgrl(X, y, difficulty, X_test = NULL, y_test = NULL, updates = 200, n_steps = 5, seed = 0, n_repeats = 50, order = "sampled")
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{difficulty}{See Usage.}
\item{X_test}{Defaults to \code{NULL}.}
\item{y_test}{Defaults to \code{NULL}.}
\item{updates}{Defaults to \code{200}.}
\item{n_steps}{Defaults to \code{5}.}
\item{seed}{Defaults to \code{0}.}
\item{n_repeats}{Defaults to \code{50}.}
\item{order}{Defaults to \code{"sampled"}.}
}
\description{
Part of the prgrl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{curriculum_error}, \code{baseline_error}, \code{improvement}, \code{curriculum_errors}, \code{baseline_errors}, \code{held_out}, \code{updates}, \code{order}, \code{n_repeats}, \code{lambdas}, \code{weights}, \code{entropies}, \code{is_curriculum}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
