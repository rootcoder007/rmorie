% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tps_sp_grad}
\alias{.tps_sp_grad}
\title{Internal helper: Tps Sp Grad}
\usage{
.tps_sp_grad(F_, dx, dy)
}
\description{
Internal helper: Tps Sp Grad
}
\value{
A list with \code{gx}, \code{gy}.
}
\keyword{internal}
\examples{
.tps_sp_grad(F_ = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), dx = c(1, 2, 3, 4, 5, 6, 7, 8), dy = c(1, 2, 3, 4, 5, 6, 7, 8))
}
