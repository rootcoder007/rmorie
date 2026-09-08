% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfLin}
\alias{alfLin}
\title{Dense projection of a vector; W is (n_out x n_in)}
\usage{
alfLin(v, W, b = NULL) 
}
\arguments{
\item{v}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{W}{A matrix; passed to \code{\%*\%}.}
\item{b}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the alf_core implementation. Called by \code{Alfbkb}, \code{Alfdgram}, \code{Alfembed} and 11 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{o}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alfLin(V, V)
}
