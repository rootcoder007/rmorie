% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.morie_did}
\alias{print.morie_did}
\title{Print method for \code{morie_did} objects}
\usage{
\method{print}{morie_did}(x, ...) 
}
\arguments{
\item{x}{A \code{morie_did} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Print method for \code{morie_did} objects
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
rmorie:::print.morie_did(D)
}
