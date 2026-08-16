% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_fitpred}
\alias{.morie_jo_fitpred}
\title{.morie_jo_fitpred}
\usage{
.morie_jo_fitpred(rows, y, newrow)
}
\arguments{
\item{rows}{A matrix; passed to \code{nrow}.}
\item{y}{Passed to \code{.morie_jo_ols}.}
\item{newrow}{See Usage.}
}
\description{
A step of the ts_joseph implementation. Called by \code{morie_dirmulti}, \code{morie_dirrec}, \code{morie_recmulti}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_jo_fitpred(rows = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), y = c(1, 2, 3, 4, 5, 6, 7, 8), newrow = c(1, 2, 3, 4, 5, 6, 7, 8))
}
