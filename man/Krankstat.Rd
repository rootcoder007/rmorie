% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Krankstat}
\alias{Krankstat}
\title{General k-sample rank statistic -- eqs. (10.5.1)-(10.5.2), pp. 362-363}
\usage{
Krankstat(samples, scores = NULL) 
}
\description{
General k-sample rank statistic -- eqs. (10.5.1)-(10.5.2), pp. 362-363
}
\arguments{
\item{samples}{Argument \code{samples}; see Usage.}
\item{scores}{Argument \code{scores}; see Usage.}
}
\value{
A list with \code{q}, \code{statistic}, \code{df}, \code{p_value}, \code{abar}, \code{score_sums}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Krankstat(V)
}
