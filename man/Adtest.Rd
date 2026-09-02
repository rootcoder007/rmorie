% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Adtest}
\alias{Adtest}
\title{Anderson-Darling W_n^2 -- eq. (4.7.1), p. 138; Table 4.7.1, p. 140}
\usage{
Adtest(x, cdf, case = "specified", alpha = 0.05) 
}
\description{
Anderson-Darling W_n^2 -- eq. (4.7.1), p. 138; Table 4.7.1, p. 140
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{cdf}{Argument \code{cdf}; see Usage.}
\item{case}{Argument \code{case}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{statistic}, \code{astar}, \code{crit}, \code{reject}, \code{z}, \code{n}, \code{case}, \code{alpha}.
}
\keyword{internal}
\examples{
set.seed(2)
rmorie:::Adtest(rnorm(30), cdf = pnorm)
}
