% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MeanSum}
\alias{MeanSum}
\title{Eq (3.13): mu_y = mu_x + mu_eta.  Linearity of expectation needs no}
\usage{
MeanSum(...) 
}
\arguments{
\item{...}{Passed through.}
}
\description{
independence -- that is what separates it from eq (3.14).
}
\value{
A list with \code{mean}, \code{component_means}, \code{n_processes}, \code{method}.
}
\keyword{internal}
\examples{
MeanSum(c(1, 3), c(10, 20), 0.5)
}
