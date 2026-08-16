% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.draw_mean}
\alias{.abcgp.draw_mean}
\title{.abcgp.draw_mean}
\usage{
.abcgp.draw_mean(mu, cov, S, e)
}
\arguments{
\item{mu}{A vector; its length is taken.}
\item{cov}{Numeric; combined arithmetically in the body.}
\item{S}{Numeric; combined arithmetically in the body.}
\item{e}{Passed to \code{.ghc_norm}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
