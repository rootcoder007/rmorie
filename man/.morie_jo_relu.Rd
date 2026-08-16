% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_relu}
\alias{.morie_jo_relu}
\title{.morie_jo_relu}
\usage{
.morie_jo_relu(t)
}
\arguments{
\item{t}{See Usage.}
}
\description{
A step of the ts_joseph implementation. Called by \code{.morie_jo_resblock}, \code{morie_itrans}, \code{morie_tsmixer}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_jo_relu(V)
}
