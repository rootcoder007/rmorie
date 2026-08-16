% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.mvn_logpdf}
\alias{.abcgp.mvn_logpdf}
\title{.abcgp.mvn_logpdf}
\usage{
.abcgp.mvn_logpdf(y, mu, cov)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{cov}{Passed to \code{.abcgp.chol}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}, \code{.abcgp.synthetic_log_likelihood}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
