% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_bet_proof_interval}
\alias{morie_bet_proof_interval}
\title{morie_bet_proof_interval}
\usage{
morie_bet_proof_interval(x, level = 0.95, lower_bound = 0, min_width = NULL) 
}
\arguments{
\item{x}{Passed to \code{morie_truncated_normal_interval}.}
\item{level}{Passed to \code{morie_truncated_normal_interval}. Defaults to \code{0.95}.}
\item{lower_bound}{Passed to \code{morie_truncated_normal_interval}. Defaults to \code{0}.}
\item{min_width}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bndpcb_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{lower}, \code{upper}, \code{width}, \code{empty}, \code{min_width}, \code{naive_width}, \code{naive_empty}, \code{widened}, \code{method}.
}
\keyword{internal}
\examples{
morie_bet_proof_interval(x = 5L)
}
