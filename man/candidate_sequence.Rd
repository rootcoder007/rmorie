% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{candidate_sequence}
\alias{candidate_sequence}
\title{candidate_sequence}
\usage{
candidate_sequence(A, W, g_models) 
}
\arguments{
\item{A}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{W}{A matrix; passed to \code{as.matrix}.}
\item{g_models}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the tlctmle_native implementation. Called by \code{ctmle}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
candidate_sequence(A = c(1, 2, 3, 4, 5, 6, 7, 8), W = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2),
  g_models = list(a = 1, b = 2))
}
