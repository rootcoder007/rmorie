% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bio_labels}
\alias{bio_labels}
\title{bio_labels}
\usage{
bio_labels(types) 
}
\arguments{
\item{types}{Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the benRea_native implementation. Called by \code{ner_decode}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
bio_labels(V)
}
