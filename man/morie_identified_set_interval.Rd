% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_identified_set_interval}
\alias{morie_identified_set_interval}
\title{morie_identified_set_interval}
\usage{
morie_identified_set_interval(phi_hat, half_width)
}
\arguments{
\item{phi_hat}{See Usage.}
\item{half_width}{See Usage.}
}
\description{
A step of the bndbye_native implementation. Called by \code{morie_compare_sets}. See the file header for the source the module follows.
}
\value{
A list with \code{lower}, \code{upper}, \code{width}, \code{phi_hat}.
}
\keyword{internal}
\examples{
morie_identified_set_interval(phi_hat = c(1, 2, 3, 4, 5, 6, 7, 8), half_width = 5L)
}
