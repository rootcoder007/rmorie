% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.changepoints}
\alias{.changepoints}
\title{.changepoints}
\usage{
.changepoints(t, n.cp, range = 0.8, cps = NULL)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{n.cp}{Coerced to integer by the body, with \code{as.integer}.}
\item{range}{Numeric; combined arithmetically in the body. Defaults to \code{0.8}.}
\item{cps}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the prphet_native implementation. Called by \code{morie_prphet_fit}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.changepoints(t = c(1, 2, 3, 4, 5, 6, 7, 8), n.cp = 5L)
}
