% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{reconstruct}
\alias{reconstruct}
\title{reconstruct}
\usage{
reconstruct(model)
}
\arguments{
\item{model}{A list; the body reads \code{$D}, \code{$S}, \code{$T} from it.}
}
\description{
A step of the lsa_native implementation. Called by \code{Singsd}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
X <- matrix(c(2, 0, 1, 0, 3, 1, 1, 1, 0, 0, 2, 1), nrow = 3, byrow = TRUE)
model <- lsa_decompose(X, k_dim = 2)
approx <- reconstruct(model)
dim(approx)
}
