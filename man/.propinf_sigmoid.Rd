% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_sigmoid}
\alias{.propinf_sigmoid}
\title{.propinf_sigmoid}
\usage{
.propinf_sigmoid(z)
}
\arguments{
\item{z}{Numeric; passed to \code{exp}.}
}
\description{
A step of the propinf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.propinf_sigmoid(z = 5L)
}
