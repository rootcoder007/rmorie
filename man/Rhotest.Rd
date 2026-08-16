% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rhotest}
\alias{Rhotest}
\title{Test of zero Spearman correlation -- Secs. 11.3.2-11.3.3, pp. 412-413}
\usage{
Rhotest(r, n, alternative = "two-sided")
}
\description{
Test of zero Spearman correlation -- Secs. 11.3.2-11.3.3, pp. 412-413
}
\value{
A list with \code{z}, \code{p_normal}, \code{t}, \code{df}, \code{p_value}, \code{var}, \code{n}.
}
\keyword{internal}
\examples{
Rhotest(r = 0.45, n = 20)
}
