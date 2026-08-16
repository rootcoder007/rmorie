% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trmwgt_cv}
\alias{.trmwgt_cv}
\title{.trmwgt_cv}
\usage{
.trmwgt_cv(w)
}
\arguments{
\item{w}{A vector; its length is taken.}
}
\description{
A step of the Trmwgt implementation. Called by \code{Trmwgt}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.trmwgt_cv(V)
}
