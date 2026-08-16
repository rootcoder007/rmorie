% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trimit_deff}
\alias{.trimit_deff}
\title{.trimit_deff}
\usage{
.trimit_deff(w)
}
\arguments{
\item{w}{A vector; its length is taken.}
}
\description{
A step of the Trimit implementation. Called by \code{Trimit}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.trimit_deff(V)
}
