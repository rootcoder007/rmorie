% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_backoff}
\alias{morie_burkov_backoff}
\title{N-gram backoff with per-level discount (Burkov Ch 2)}
\usage{
morie_burkov_backoff(counts_by_order, alpha = 0.4) 
}
\arguments{
\item{counts_by_order}{List of `c(count_ngram, count_prefix)` pairs,   highest order first.}
\item{alpha}{Per-level discount in (0, 1\\].}
}
\description{
N-gram backoff with per-level discount (Burkov Ch 2)
}
\value{
The value of \code{stop}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_burkov_backoff(D)
}
