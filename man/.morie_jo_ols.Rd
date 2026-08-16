% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_ols}
\alias{.morie_jo_ols}
\title{.morie_jo_ols}
\usage{
.morie_jo_ols(x, y)
}
\arguments{
\item{x}{A matrix; indexed by row and column.}
\item{y}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the ts_joseph implementation. Called by \code{.morie_jo_fitpred}, \code{morie_adfur}, \code{morie_quantreg}. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_jo_solve}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_jo_ols(V, V)
}
