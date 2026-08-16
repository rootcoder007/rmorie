% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{group_privacy}
\alias{group_privacy}
\title{Proposition 1.9: groups of size k cost k^2 rho}
\usage{
group_privacy(rho, k)
}
\arguments{
\item{rho}{zCDP parameter, must be positive.}
\item{k}{Group size, must be at least 1.}
}
\description{
Proposition 1.9: groups of size k cost k^2 rho
}
\value{
A list with \code{rho}, \code{k}, and a \code{growth}   string noting the quadratic, exactly tight bound.
}
\keyword{internal}
\examples{
group_privacy(rho = 0.5, k = 5L)
}
