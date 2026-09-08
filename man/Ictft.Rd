% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ictft}
\alias{Ictft}
\title{Eq (3.77): the 1/(2 pi) belongs to the omega form only.  Getting that}
\usage{
Ictft(X, t, omega = NULL, f = NULL) 
}
\arguments{
\item{X}{Coerced to complex by the body, with \code{as.complex}.}
\item{t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{omega}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{f}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
factor wrong scales the synthesis by 6.28, so the branch is explicit.
}
\value{
A list with \code{x}, \code{t}, \code{variable}, \code{scale}, \code{method}.
}
\keyword{internal}
\examples{
grid <- seq(-1, 1, length.out = 401)
X <- rep(complex(real = 1, imaginary = 0), 401)
Ictft(X, t = 0, omega = grid)
}
