% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lstm_step}
\alias{lstm_step}
\title{One LSTM cell step, gates in the order i, f, g, o}
\usage{
lstm_step(x, h, c, Wx, Wh, b)
}
\arguments{
\item{x}{See Usage.}
\item{h}{See Usage.}
\item{c}{See Usage.}
\item{Wx}{See Usage.}
\item{Wh}{See Usage.}
\item{b}{See Usage.}
}
\description{
Part of the elmo_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{h}, \code{c}.
}
\keyword{internal}
