% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_fxw_grid}
\alias{.hrz3_fxw_grid}
\title{Bivariate kernel density of (X, W) on the grid, mass-corrected}
\usage{
.hrz3_fxw_grid(u, v, z, wq, h)
}
\arguments{
\item{u}{A vector; its length is taken.}
\item{v}{A vector; its length is taken.}
\item{z}{Passed to \code{.hrz3_kmat}.}
\item{wq}{Passed to \code{outer}.}
\item{h}{Numeric; combined arithmetically in the body.}
}
\description{
A Gaussian kernel has unbounded support, so on the compact [0, 1]^2 a fixed share of its mass falls outside and the raw estimate does NOT integrate to one.  Horowitz (2009) p. 173 requires a compactly supported kernel (HH5) and notes boundary effects "can be accommodated by replacing the kernel K with a boundary kernel". Renormalising the discretised density to unit mass on [0, 1]^2 is such a correction, and it makes mass == 1 an exact identity that fails loudly if the 1/(n h^2) constant is mis-wired.
}
\value{
A list with \code{f}, \code{mass}.
}
\keyword{internal}
\examples{
.hrz3_fxw_grid(u = c(1, 2, 3, 4, 5, 6, 7, 8), v = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1, 2, 3, 4, 5, 6, 7, 8), wq = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5)
}
