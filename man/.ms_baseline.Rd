% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ms_baseline}
\alias{.ms_baseline}
\title{Breslow cumulative baseline hazard, what survival::basehaz returns}
\usage{
.ms_baseline(time, event, X, beta)
}
\arguments{
\item{time}{Passed to \code{.ms_check}.}
\item{event}{Passed to \code{.ms_check}.}
\item{X}{A matrix; passed to \code{nrow}.}
\item{beta}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the survival_more implementation. Called by \code{Coxsnell}, \code{Martingale}. See the file header for the source the module follows.
}
\value{
A list with \code{time}, \code{cumhaz}, \code{weight}.
}
\keyword{internal}
\examples{
.ms_baseline(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5)
}
