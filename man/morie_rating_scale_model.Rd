% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rating_scale_model}
\alias{morie_rating_scale_model}
\title{morie_rating_scale_model}
\usage{
morie_rating_scale_model(theta, b, tau) 
}
\description{
morie_rating_scale_model
}
\arguments{
\item{theta}{Argument \code{theta}; see Usage.}
\item{b}{Argument \code{b}; see Usage.}
\item{tau}{Argument \code{tau}; see Usage.}
}
\value{
A list with \code{p}, \code{expected}, \code{info}, \code{test_expected}, \code{test_info}, \code{theta}, \code{b}, \code{tau}, \code{ncat}, \code{n}, \code{k}, \code{method}.
}
\keyword{internal}
\examples{
morie_rating_scale_model(theta = 0.5, b = 5L, tau = 0.5)
}
