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
\item{x}{A vector; its length is taken.}
\item{h}{A vector; its length is taken.}
\item{c}{A vector; its length is taken.}
\item{Wx}{A matrix; passed to \code{\%*\%}.}
\item{Wh}{A matrix; passed to \code{\%*\%}.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the elmo_native implementation. Called by \code{bilm_forward}. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{c}.
}
\keyword{internal}
\examples{
lstm_step(x = c(1, 2, 3, 4, 5, 6, 7, 8), h = c(1, 2, 3, 4, 5, 6, 7, 8), c = c(1, 2, 3, 4, 5, 6, 7, 8), Wx = c(1, 2, 3, 4, 5, 6, 7, 8), Wh = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
}
