% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.morie_knox}
\alias{print.morie_knox}
\title{Print method for \code{morie_knox} objects}
\usage{
\method{print}{morie_knox}(x, ...) 
}
\arguments{
\item{x}{A \code{morie_knox} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Print method for \code{morie_knox} objects
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
print.morie_knox(D)
}
