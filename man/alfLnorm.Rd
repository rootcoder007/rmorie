% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfLnorm}
\alias{alfLnorm}
\title{Layer normalisation over the channel axis of one vector.  Uses the}
\usage{
alfLnorm(v, g = NULL, b = NULL, eps = 1e-05)
}
\arguments{
\item{v}{A vector; its length is taken.}
\item{g}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{b}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.}
}
\description{
population variance (divide by n), matching the Python arm.
}
\value{
The value of \code{o}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alfLnorm(V)
}
