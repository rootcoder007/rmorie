% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_piecewise_trend}
\alias{morie_prphet_piecewise_trend}
\title{morie_prphet_piecewise_trend}
\usage{
morie_prphet_piecewise_trend(t, k.rate, m.off, deltas, cps) 
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{k.rate}{Numeric; combined arithmetically in the body.}
\item{m.off}{Numeric; combined arithmetically in the body.}
\item{deltas}{Numeric; combined arithmetically in the body.}
\item{cps}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the prphet_native implementation. Called by \code{.prnFil_simulate_future_trend}, \code{morie_prphet_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
morie_prphet_piecewise_trend(t = c(1, 2, 3, 4, 5, 6, 7, 8), k.rate = c(1, 2, 3, 4, 5, 6, 7, 8),
  m.off = c(1, 2, 3, 4, 5, 6, 7, 8), deltas = c(1, 2, 3, 4, 5, 6, 7, 8),
  cps = c(1, 2, 3, 4, 5, 6, 7, 8))
}
