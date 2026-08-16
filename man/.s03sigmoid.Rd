% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03sigmoid}
\alias{.s03sigmoid}
\title{.s03sigmoid}
\usage{
.s03sigmoid(z)
}
\arguments{
\item{z}{Numeric; passed to \code{exp}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.dnnact}, \code{.dw_skipgram}, \code{.s03swish} and 21 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.s03sigmoid(z = 5L)
}
