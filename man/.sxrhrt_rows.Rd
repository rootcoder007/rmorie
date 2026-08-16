% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sxrhrt_rows}
\alias{.sxrhrt_rows}
\title{.sxrhrt_rows}
\usage{
.sxrhrt_rows(x)
}
\arguments{
\item{x}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the sxrhrt_native implementation. Called by \code{morie_sxrhrt_sex_specific_h2}. See the file header for the source the module follows.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sxrhrt_rows(V)
}
