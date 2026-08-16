% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.expected_error}
\alias{.abcgp.expected_error}
\title{.abcgp.expected_error}
\usage{
.abcgp.expected_error(alphas, tau, n_grid = 101)
}
\arguments{
\item{alphas}{A vector; its length is taken.}
\item{tau}{See Usage.}
\item{n_grid}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{101}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
