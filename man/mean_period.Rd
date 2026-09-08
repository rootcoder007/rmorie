% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mean_period}
\alias{mean_period}
\title{mean_period}
\usage{
mean_period(y, dt = 1) 
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{dt}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the lyapun_native implementation. Called by \code{divergence_curve}, \code{morie_lyapun}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
y <- sin(2 * pi * (1:400) / 25)
mean_period(y)
}
