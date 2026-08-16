% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{resolve_synthetic_name_map}
\alias{resolve_synthetic_name_map}
\title{resolve_synthetic_name_map}
\usage{
resolve_synthetic_name_map(name_map, profile)
}
\arguments{
\item{name_map}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{profile}{Passed to \code{morie_default_synthetic_name_map}.}
}
\description{
A step of the synthetic implementation. Called by \code{morie_generate_synthetic_data}. See the file header for the source the module follows.
}
\value{
The value of \code{resolved}, as built in the body.
}
\keyword{internal}
