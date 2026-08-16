% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_truncated_normal_interval}
\alias{morie_truncated_normal_interval}
\title{morie_truncated_normal_interval}
\usage{
morie_truncated_normal_interval(x, level = 0.95, lower_bound = 0)
}
\arguments{
\item{x}{See Usage.}
\item{level}{Defaults to \code{0.95}.}
\item{lower_bound}{Defaults to \code{0}.}
}
\description{
A step of the bndpcb_native implementation. Called by \code{morie_bet_proof_interval}. See the file header for the source the module follows.
}
\value{
A list with \code{lower}, \code{upper}, \code{width}, \code{empty}, \code{z}, \code{level}, \code{note}.
}
\keyword{internal}
\examples{
morie_truncated_normal_interval(x = 5L)
}
