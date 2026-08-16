% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.gabc_log_likelihood}
\alias{.abcgp.gabc_log_likelihood}
\title{.abcgp.gabc_log_likelihood}
\usage{
.abcgp.gabc_log_likelihood(sim, obs, theta, n_sim = 50, epsilon = 1, summary = NULL, kernel = "gaussian", seed = 0, bootstrap = 25)
}
\arguments{
\item{sim}{See Usage.}
\item{obs}{Passed to \code{.abcgp.summarise}.}
\item{theta}{See Usage.}
\item{n_sim}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50}.}
\item{epsilon}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{summary}{Passed to \code{.abcgp.summarise}.}
\item{kernel}{One of \code{"gaussian"}, \code{"uniform"}. Defaults to \code{"gaussian"}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{bootstrap}{Defaults to \code{25}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.history_match}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
