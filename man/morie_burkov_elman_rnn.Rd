% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_elman_rnn}
\alias{morie_burkov_elman_rnn}
\title{One step of the Elman RNN (Burkov Ch 3)}
\usage{
morie_burkov_elman_rnn(x_t, h_prev, Wh, Wx, Wy, bh, by)
}
\arguments{
\item{x_t}{Input.}
\item{h_prev}{Previous hidden state.}
\item{Wh,Wx,Wy}{Weight matrices.}
\item{bh,by}{Biases.}
}
\description{
One step of the Elman RNN (Burkov Ch 3)
}
\value{
A list with \code{h}, \code{y}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
