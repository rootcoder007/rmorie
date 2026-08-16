% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.design_from_prior}
\alias{.abcgp.design_from_prior}
\title{.abcgp.design_from_prior}
\usage{
.abcgp.design_from_prior(n, prior_ppf, dim = NULL, skip = 1)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{prior_ppf}{A vector; its length is taken and its elements indexed.}
\item{dim}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{skip}{Passed to \code{.abcgp.sobol_sequence}. Defaults to \code{1}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.history_match}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.abcgp.design_from_prior(n = 5L, prior_ppf = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)))
}
