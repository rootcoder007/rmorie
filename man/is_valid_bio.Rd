% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{is_valid_bio}
\alias{is_valid_bio}
\title{is_valid_bio}
\usage{
is_valid_bio(path)
}
\arguments{
\item{path}{See Usage.}
}
\description{
A step of the benRea_native implementation. Called by \code{ner_decode}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
is_valid_bio(V)
}
