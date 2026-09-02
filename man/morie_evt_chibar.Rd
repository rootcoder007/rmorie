% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_chibar}
\alias{morie_evt_chibar}
\title{Empirical chibar(u) (Coles 2001 sec. 8.4 p.164)}
\usage{
morie_evt_chibar(x, y, u_grid = seq(0.5, 0.95, length.out = 20)) 
}
\arguments{
\item{u_grid}{quantile grid}

\item{x}{Argument \code{x}; see Usage.}

\item{y}{Argument \code{y}; see Usage.}
}
\description{
Empirical chibar(u) (Coles 2001 sec. 8.4 p.164)
}
\value{
A vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_evt_chibar(V, V)
}
