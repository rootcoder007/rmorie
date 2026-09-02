% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Taunull}
\alias{Taunull}
\title{Null distribution of S = P - Q -- Sec. 11.2.1, p. 395}
\usage{
Taunull(n, s = NULL) 
}
\description{
Null distribution of S = P - Q -- Sec. 11.2.1, p. 395
}
\arguments{
\item{n}{Argument \code{n}; see Usage.}
\item{s}{Argument \code{s}; see Usage.}
}
\value{
A list with \code{support}, \code{pmf}, \code{pmf_s}, \code{cdf_s}, \code{sf_s}, \code{var_tau}, \code{var_s}, \code{n}.
}
\keyword{internal}
\examples{
Taunull(n = 5L)
}
