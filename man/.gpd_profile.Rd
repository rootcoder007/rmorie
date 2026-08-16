% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpd_profile}
\alias{.gpd_profile}
\title{Grimshaw\\'s reparametrisation: given t = xi/sigma, xi and sigma}
\usage{
.gpd_profile(y, t)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{t}{Numeric; passed to \code{abs}.}
}
\description{
follow Part of the evpot implementation; see the file header for the source it follows.
}
\value{
A list with \code{xi}, \code{sigma}, \code{g}.
}
\keyword{internal}
\examples{
.gpd_profile(y = c(1, 2, 3, 4, 5, 6, 7, 8), t = 5L)
}
