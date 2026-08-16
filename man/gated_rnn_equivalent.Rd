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
\item{x}{A vector; its length is taken and its elements indexed.}
\item{w}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the mambss_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{g}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gated_rnn_equivalent(V, V)
}
