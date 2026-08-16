% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gated_rnn_equivalent}
\alias{gated_rnn_equivalent}
\title{gated_rnn_equivalent}
\usage{
gated_rnn_equivalent(x, w, b = 0)
}
\arguments{
\item{x}{See Usage.}
\item{w}{See Usage.}
\item{b}{Defaults to \code{0}.}
}
\description{
Part of the mambss_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{h}, \code{g}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gated_rnn_equivalent(V, V)
}
