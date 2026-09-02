% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lillienorm}
\alias{Lillienorm}
\title{Lilliefors test for normality -- Sec. 4.5, p. 126; Table O}
\usage{
Lillienorm(x, alpha = 0.05) 
}
\description{
Lilliefors test for normality -- Sec. 4.5, p. 126; Table O
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{statistic}, \code{dcrit}, \code{reject}, \code{mean}, \code{sd}, \code{n}, \code{n_table}, \code{alpha}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Lillienorm(V)
}
