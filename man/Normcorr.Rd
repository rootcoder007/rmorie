% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Normcorr}
\alias{Normcorr}
\title{Fieller-Hartley-Pearson normal-scores correlation -- Sec. 11.5, p. 422}
\usage{
Normcorr(x, y, rho = 0, nodes = 4001)
}
\description{
Fieller-Hartley-Pearson normal-scores correlation -- Sec. 11.5, p. 422
}
\value{
A list with \code{statistic}, \code{zf}, \code{mean_zf}, \code{var_zf}, \code{z}, \code{p_value}, \code{scores}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Normcorr(V, V)
}
