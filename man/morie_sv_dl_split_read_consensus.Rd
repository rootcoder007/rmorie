% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_split_read_consensus}
\alias{morie_sv_dl_split_read_consensus}
\title{Gapless majority-vote consensus over the aligned reads. starts}
\usage{
morie_sv_dl_split_read_consensus(reads, starts = NULL) 
}
\arguments{
\item{reads}{Coerced to character by the body, with \code{as.character}.}
\item{starts}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
places each read in a common frame. Returns list(consensus, start).
}
\value{
A list with \code{consensus}, \code{start}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sv_dl_split_read_consensus(V)
}
