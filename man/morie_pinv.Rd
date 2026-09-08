% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pinv}
\alias{morie_pinv}
\title{morie_pinv}
\usage{
morie_pinv(A, rcond = 1e-15) 
}
\description{
morie_pinv
}
\arguments{
\item{A}{Argument \code{A}; see Usage.}
\item{rcond}{Argument \code{rcond}; see Usage.}
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::morie_pinv(V)
}
