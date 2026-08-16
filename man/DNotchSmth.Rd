% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DNotchSmth}
\alias{DNotchSmth}
\title{Eq (4.23) (Lehner and Rangayyan): s(n) = sum_k p^2(n-k+1) (M - k + 1)}
\usage{
DNotchSmth(p, mwin = 16)
}
\arguments{
\item{p}{A vector; its length is taken and its elements indexed.}
\item{mwin}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{16}.}
}
\description{
Squaring discards the sign of the curvature; the linearly decaying weight smooths while keeping the response prompt at the newest sample.  s(n) has two peaks per cycle: the upstroke onset, then the dicrotic notch.
}
\value{
A list with \code{s}, \code{weights}, \code{mwin}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DNotchSmth(V)
}
