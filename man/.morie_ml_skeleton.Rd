% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_skeleton}
\alias{.morie_ml_skeleton}
\title{.morie_ml_skeleton}
\usage{
.morie_ml_skeleton(edges)
}
\arguments{
\item{edges}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the causal_molak implementation. Called by \code{morie_mectest}. See the file header for the source the module follows.
}
\value{
A vector, from \code{sort}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_ml_skeleton(M)
}
