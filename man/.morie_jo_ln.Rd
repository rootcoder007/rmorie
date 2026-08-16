% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_ln}
\alias{.morie_jo_ln}
\title{.morie_jo_ln}
\usage{
.morie_jo_ln(v, eps = 1e-05)
}
\arguments{
\item{v}{A vector; its length is taken.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.}
}
\description{
A step of the ts_joseph implementation. Called by \code{.morie_jo_resblock}, \code{morie_itrans}, \code{morie_tftnet} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_jo_ln(V)
}
