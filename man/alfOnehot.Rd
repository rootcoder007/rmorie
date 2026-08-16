% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfOnehot}
\alias{alfOnehot}
\title{One-hot encoding with nearest bin -- Algorithm 5.  Ties go to the}
\usage{
alfOnehot(x, bins)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{bins}{A vector; its length is taken.}
}
\description{
lowest index, matching arg min in the pseudocode and which.min here.
}
\value{
The value of \code{p}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alfOnehot(V, V)
}
