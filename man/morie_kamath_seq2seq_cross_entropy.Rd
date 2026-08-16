% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_seq2seq_cross_entropy}
\alias{morie_kamath_seq2seq_cross_entropy}
\title{morie_kamath_seq2seq_cross_entropy}
\usage{
morie_kamath_seq2seq_cross_entropy(y, c, U = NULL)
}
\arguments{
\item{y}{Target token indices, 0-based.}
\item{U}{Optional length pin.}
}
\description{
morie_kamath_seq2seq_cross_entropy
}
\value{
A list with \code{estimate}, \code{per_step}, \code{mean_loss}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_seq2seq_cross_entropy(c(0, 1), rbind(c(0.5, 0.5), 
    c(0.25, 0.75)))
}
