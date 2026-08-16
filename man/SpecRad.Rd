% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpecRad}
\alias{SpecRad}
\title{The SAR bound |rho| < 1/rho(W) comes from the non-singularity}
\usage{
SpecRad(g, iters = 400L)
}
\arguments{
\item{g}{Passed to \code{.morie_spx_chkw}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{400L}.}
}
\description{
condition in the PROSE of Sec. 6.2.2.1, p. 336 (1/theta_min < rho < 1/theta_max, after Haining 1990 p. 82) -- NOT from eq (6.48), which is the CAR information matrix.  Anything in this package citing (6.48) for the rho interval is miscited.
}
\value{
A list with \code{rho}, \code{dominant_eigenvalue}, \code{eigenvector}, \code{sar_rho_bound}, \code{symmetric}, \code{iterations}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
W <- matrix(0, 5, 5)
W[abs(row(W) - col(W)) == 1] <- 1
SpecRad(W)
}
