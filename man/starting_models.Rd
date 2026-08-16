% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{starting_models}
\alias{starting_models}
\title{starting_models}
\usage{
starting_models(d, D, s)
}
\arguments{
\item{d}{See Usage.}
\item{D}{See Usage.}
\item{s}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the sarimax_native implementation. Called by \code{auto_order}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
starting_models(d = 5L, D = 5L, s = 5L)
}
