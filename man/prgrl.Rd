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
\item{X}{Passed to \code{.prgrl_to_rows}.}
\item{y}{Passed to \code{.prgrl_to_vec}.}
\item{difficulty}{Passed to \code{.prgrl_to_vec}.}
\item{X_test}{Optional; may be \code{NULL}. Passed to \code{.prgrl_to_rows}.}
\item{y_test}{Passed to \code{.prgrl_to_vec}.}
\item{updates}{Passed to \code{.prgrl_perceptron}. Defaults to \code{200}.}
\item{n_steps}{Passed to \code{curriculum_schedule}. Defaults to \code{5}.}
\item{seed}{Passed to \code{.prgrl_rng}. Defaults to \code{0}.}
\item{n_repeats}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50}.}
\item{order}{One of \code{"sampled"}, \code{"sorted"}. Defaults to \code{"sampled"}.}
}
\description{
A step of the prgrl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{curriculum_error}, \code{baseline_error}, \code{improvement}, \code{curriculum_errors}, \code{baseline_errors}, \code{held_out}, \code{updates}, \code{order}, \code{n_repeats}, \code{lambdas}, \code{weights}, \code{entropies}, \code{is_curriculum}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
