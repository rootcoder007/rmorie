% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{extract_spans}
\alias{extract_spans}
\title{extract_spans}
\usage{
extract_spans(path)
}
\arguments{
\item{path}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the benRea_native implementation. Called by \code{ner_decode}, \code{span_f1}. See the file header for the source the module follows.
}
\value{
The value of \code{spans}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
extract_spans(V)
}
