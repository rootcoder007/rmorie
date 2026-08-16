% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_bio_tagging}
\alias{morie_alammar_bio_tagging}
\title{morie_alammar_bio_tagging}
\usage{
morie_alammar_bio_tagging(tokens, entity_spans, scheme = "BIO")
}
\arguments{
\item{entity_spans}{List of c(start, end, type), 0-based, end   exclusive, matching the Python mirror.}
\item{scheme}{BIO or BIOES.}
}
\description{
morie_alammar_bio_tagging
}
\value{
A list with \code{tags}, \code{n_entities}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_alammar_bio_tagging(V, D)
}
