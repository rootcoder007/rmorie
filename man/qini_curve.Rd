% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{qini_curve}
\alias{qini_curve}
\title{qini_curve}
\usage{
qini_curve(scores, priority, cost = NULL) 
}
\arguments{
\item{scores}{Passed to \code{.slvgrf_check}.}
\item{priority}{Passed to \code{.slvgrf_check}.}
\item{cost}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
A step of the slvgrf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{spend}, \code{gain}, \code{ate}, \code{n}, \code{constrained}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
qini_curve(V, V)
}
