% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.panel}
\alias{.panel}
\title{Validate a balanced panel}
\usage{
.panel(Y)
}
\arguments{
\item{Y}{Numeric matrix (n by T).}
}
\description{
Validate a balanced panel
}
\value{
A list with the matrix and its dimensions.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.panel(M)
}
