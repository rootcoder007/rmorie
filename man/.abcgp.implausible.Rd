% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.implausible}
\alias{.abcgp.implausible}
\title{.abcgp.implausible}
\usage{
.abcgp.implausible(fit, theta, threshold = 10, n_sd = 3)
}
\arguments{
\item{fit}{A list; the body reads \code{$values} from it.}
\item{theta}{Passed to \code{.abcgp.gp_predict}.}
\item{threshold}{Numeric; combined arithmetically in the body. Defaults to \code{10}.}
\item{n_sd}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.history_match}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
