% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rope}
\alias{Rope}
\title{Rotary position embedding (Su et al. 2021, RoFormer)}
\usage{
Rope(q, m, theta)
}
\description{
Rotary position embedding (Su et al. 2021, RoFormer)
}
\value{
A list with \code{q}, \code{m}, \code{norm}, \code{n}.
}
\keyword{internal}
\examples{
Rope(q = c(1, 0, 0.5, -0.5), m = 3,
     theta = 10000^(-c(0, 1) / 2))
}
