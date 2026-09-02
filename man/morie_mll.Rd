% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mll}
\alias{morie_mll}
\title{morie_mll}
\usage{
morie_mll(probs, y_true) 
}
\description{
morie_mll
}
\arguments{
\item{probs}{Argument \code{probs}; see Usage.}
\item{y_true}{Argument \code{y_true}; see Usage.}
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
S <- c("a", "b", "c")
rmorie:::morie_mll(V, S)
}
