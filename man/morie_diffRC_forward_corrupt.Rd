% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_diffRC_forward_corrupt}
\alias{morie_diffRC_forward_corrupt}
\title{Forward corruption}
\usage{
morie_diffRC_forward_corrupt(x0, alpha_bar_t, rng = NULL) 
}
\arguments{
\item{x0}{Numeric vector.}
\item{alpha_bar_t}{Numeric.}
\item{rng}{Optional generator environment.}
}
\description{
Closed form, so no simulation is needed to check it.
}
\value{
A list with \code{x_t}, \code{mean}, \code{std} and   \code{sampled}.
}
\keyword{internal}
\examples{
rng <- rmorie:::.ghc_rng(1)
r <- morie_diffRC_forward_corrupt(x0 = c(1, -1, 0.5),
                                  alpha_bar_t = 0.7, rng = rng)
str(r, max.level = 1)
}
