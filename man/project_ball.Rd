% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{project_ball}
\alias{project_ball}
\title{project_ball}
\usage{
project_ball(x, radius = 1, centre = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{radius}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{centre}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the pgdsdg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\references{
Goldstein, A. A. (1964) "Convex programming in Hilbert space",
  Bulletin of the American Mathematical Society 70(5), 709-710,
  doi:10.1090/S0002-9904-1964-11178-2.
Levitin, E. S. & Polyak, B. T. (1966) "Constrained minimization
  methods", USSR Computational Mathematics and Mathematical Physics
  6(5), 1-50, doi:10.1016/0041-5553(66)90114-5.
Beck, A. & Teboulle, M. (2009) "A fast iterative shrinkage-
  thresholding algorithm for linear inverse problems", SIAM Journal
  on Imaging Sciences 2(1), 183-202, doi:10.1137/080716542.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
project_ball(V)
}
