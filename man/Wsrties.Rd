% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wsrties}
\alias{Wsrties}
\title{Tie-corrected Var[T+] -- eqs. (5.7.10)-(5.7.11), p. 203}
\usage{
Wsrties(d, m0 = 0) 
}
\description{
Tie-corrected Var[T+] -- eqs. (5.7.10)-(5.7.11), p. 203
}
\arguments{
\item{d}{Argument \code{d}; see Usage.}
\item{m0}{Argument \code{m0}; see Usage.}
}
\value{
A list with \code{var}, \code{var_uncorrected}, \code{correction}, \code{n}, \code{nzero}, \code{ties}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Wsrties(V)
}
