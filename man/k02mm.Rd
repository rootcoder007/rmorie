% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02mm}
\alias{k02mm}
\title{k02mm}
\usage{
k02mm(y, v, tau0) 
}
\arguments{
\item{y}{Numeric; combined arithmetically in the body.}
\item{v}{Numeric; combined arithmetically in the body.}
\item{tau0}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the k02util implementation. Called by \code{matr}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
k02mm(y = c(1, 2, 3, 4, 5, 6, 7, 8), v = c(1, 2, 3, 4, 5, 6, 7, 8), tau0 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
