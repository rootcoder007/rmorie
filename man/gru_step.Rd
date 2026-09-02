% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gru_step}
\alias{gru_step}
\title{One GRU update}
\usage{
gru_step(x, h, Wz, Uz, Wr, Ur, Wh, Uh) 
}
\arguments{
\item{x}{Input vector.}
\item{h}{Hidden state.}
\item{Wz}{Update gate input weight matrix.}
\item{Uz}{Update gate recurrent weight matrix.}
\item{Wr}{Reset gate input weight matrix.}
\item{Ur}{Reset gate recurrent weight matrix.}
\item{Wh}{Candidate input weight matrix.}
\item{Uh}{Candidate recurrent weight matrix.}
}
\description{
One GRU update
}
\value{
New hidden state.
}
\keyword{internal}
\examples{
set.seed(1)
h <- c(0, 0)
x <- c(1, -1)
W <- matrix(rnorm(4), 2, 2)
gru_step(x, h, W, W, W, W, W, W)
}
