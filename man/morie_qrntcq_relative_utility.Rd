% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_qrntcq_relative_utility}
\alias{morie_qrntcq_relative_utility}
\title{morie_qrntcq_relative_utility}
\usage{
morie_qrntcq_relative_utility(t.R.a, t.R.b, t.Q = 3, generation.time = NULL, infected.fraction = NULL)
}
\arguments{
\item{t.R.a}{Numeric; combined arithmetically in the body.}
\item{t.R.b}{Numeric; combined arithmetically in the body.}
\item{t.Q}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{generation.time}{Defaults to \code{NULL}.}
\item{infected.fraction}{Defaults to \code{NULL}.}
}
\description{
A step of the qrntcq_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{relative.utility}, \code{utility.a}, \code{utility.b}, \code{efficacy.a}, \code{efficacy.b}, \code{independent.of.infected.fraction}, \code{note}.
}
\keyword{internal}
\examples{
morie_qrntcq_relative_utility(t.R.a = 5L, t.R.b = 5L)
}
