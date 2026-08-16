% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpoF_sigmoid}
\alias{.dpoF_sigmoid}
\title{.dpoF_sigmoid}
\usage{
.dpoF_sigmoid(z)
}
\arguments{
\item{z}{Numeric; passed to \code{exp}.}
}
\description{
A step of the dpoF_native implementation. Called by \code{morie_dpoF}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.dpoF_sigmoid(z = 5L)
}
