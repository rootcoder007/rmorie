% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_ripley_weights}
\alias{.schab_ripley_weights}
\title{.schab_ripley_weights}
\usage{
.schab_ripley_weights(points, region, radii)
}
\arguments{
\item{points}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{region}{A vector; indexed elementwise.}
\item{radii}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_cross_k}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
