% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_greedy_decoding}
\alias{morie_alammar_greedy_decoding}
\title{Greedy and sampled decoding (Alammar Ch 6)}
\usage{
morie_alammar_greedy_decoding(logits) 
}
\arguments{
\item{logits}{Matrix, one row per step.}
}
\description{
Greedy and sampled decoding (Alammar Ch 6)
}
\value{
A list with \code{tokens}, \code{had_ties}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_greedy_decoding(V)
}
