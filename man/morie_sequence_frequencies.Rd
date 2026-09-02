% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sequence_frequencies}
\alias{morie_sequence_frequencies}
\title{morie_sequence_frequencies}
\usage{
morie_sequence_frequencies(Y) 
}
\arguments{
\item{Y}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the bnshrt_native implementation. Called by \code{morie_identified_set}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
set.seed(1)
Y <- matrix(rbinom(40, 1, 0.5), 20, 2)
morie_sequence_frequencies(Y)
}
