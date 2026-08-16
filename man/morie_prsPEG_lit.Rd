% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prsPEG_lit}
\alias{morie_prsPEG_lit}
\title{morie_prsPEG_lit}
\usage{
morie_prsPEG_lit(s)
}
\arguments{
\item{s}{A vector; its length is taken.}
}
\description{
A step of the prsPEG_native implementation. Called by \code{morie_prsPEG_opt}. See the file header for the source the module follows.
}
\value{
The value of \code{fn}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prsPEG_lit(V)
}
