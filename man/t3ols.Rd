% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{t3ols}
\alias{t3ols}
\title{t3ols}
\usage{
t3ols(X, y) 
}
\arguments{
\item{X}{A matrix; passed to \code{t}.}
\item{y}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the t3util implementation. Called by \code{giss}, \code{qsarh}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
t3ols(V, V)
}
