% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_weight_tying}
\alias{morie_burkov_weight_tying}
\title{Weight tying: logits through the shared embedding (Burkov Ch 4)}
\usage{
morie_burkov_weight_tying(h_last, E) 
}
\arguments{
\item{h_last}{Hidden state.}
\item{E}{Embedding matrix, V x d.}
}
\description{
Weight tying: logits through the shared embedding (Burkov Ch 4)
}
\value{
A list with \code{logits}, \code{estimate}, \code{vocab_size}, \code{hidden_size}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
S <- c("a", "b", "c")
morie_burkov_weight_tying(S, M)
}
