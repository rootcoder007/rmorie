% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prsPEG_star}
\alias{morie_prsPEG_star}
\title{morie_prsPEG_star}
\usage{
morie_prsPEG_star(e)
}
\arguments{
\item{e}{Passed to \code{.probe}.}
}
\description{
A step of the prsPEG_native implementation. Called by \code{morie_prsPEG_plus}. See the file header for the source the module follows.
}
\value{
The value of \code{fn}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prsPEG_star(V)
}
