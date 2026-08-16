% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_sigmoid}
\alias{.morie_jo_sigmoid}
\title{.morie_jo_sigmoid}
\usage{
.morie_jo_sigmoid(t)
}
\arguments{
\item{t}{Numeric; passed to \code{exp}.}
}
\description{
A step of the ts_joseph implementation. Called by \code{.morie_jo_glu}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_jo_sigmoid(V)
}
