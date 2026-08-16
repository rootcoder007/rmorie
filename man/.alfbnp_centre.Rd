% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfbnp_centre}
\alias{.alfbnp_centre}
\title{.alfbnp_centre}
\usage{
.alfbnp_centre(X)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the alfbnp_native implementation. Called by \code{morie_alfbnp_af3_sample}. See the file header for the source the module follows.
}
\value{
The value of \code{X}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.alfbnp_centre(M)
}
