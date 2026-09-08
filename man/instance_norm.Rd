% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{instance_norm}
\alias{instance_norm}
\title{instance_norm}
\usage{
instance_norm(x) 
}
\arguments{
\item{x}{Passed to \code{.patcht_vec}.}
}
\description{
A step of the patchT_native implementation. Called by \code{patchtst_encode}. See the file header for the source the module follows.
}
\value{
A list with \code{normalised}, \code{mean}, \code{sd}, \code{degenerate}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
instance_norm(V)
}
