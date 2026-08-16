% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rob_design}
\alias{.rob_design}
\title{.rob_design}
\usage{
.rob_design(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the robust_native implementation. Called by \code{morie_rob_huber}, \code{morie_rob_m}, \code{morie_rob_mm} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{X}, \code{y}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rob_design(V, V)
}
