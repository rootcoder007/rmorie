% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_chi}
\alias{morie_evt_chi}
\title{Empirical chi(u) tail dependence (Coles 2001 sec. 8.4 p.164)}
\usage{
morie_evt_chi(x, y, u = 0.95)
}
\arguments{
\item{x,y}{equal-length series; u quantile level}
}
\description{
Empirical chi(u) tail dependence (Coles 2001 sec. 8.4 p.164)
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_evt_chi(V, V)
}
