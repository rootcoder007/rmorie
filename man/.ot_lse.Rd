% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_lse}
\alias{.ot_lse}
\title{.ot_lse}
\usage{
.ot_lse(v)
}
\arguments{
\item{v}{Numeric; passed to \code{max}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{.ot_sinkhorn}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ot_lse(V)
}
