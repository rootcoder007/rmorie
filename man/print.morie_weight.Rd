% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.morie_weight}
\alias{print.morie_weight}
\title{Print method for \code{morie_weight} objects}
\usage{
\method{print}{morie_weight}(x, ...) 
}
\arguments{
\item{x}{A \code{morie_weight} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Print method for \code{morie_weight} objects
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
rmorie:::print.morie_weight(D)
}
